import torch
import random
import math
import re
from tqdm.auto import tqdm
from model_lib import ModelUtils


def top_k_top_p_filtering(logits, top_k=0, top_p=0.0, filter_value=-1e9):
    """
    Filter a distribution of logits using top-k and/or nucleus (top-p) filtering
    logits: 1D tensor (vocab,)
    Returns logits with filtered positions set to filter_value.
    """
    # Clone to avoid in-place
    logits = logits.clone()
    vocab_size = logits.size(0)

    if top_k > 0:
        # Remove all tokens with a rank below top_k
        top_k = min(max(top_k, 1), vocab_size)
        vals, _ = torch.topk(logits, top_k)
        min_val = vals[-1]
        logits[logits < min_val] = filter_value

    if top_p > 0.0 and top_p < 1.0:
        sorted_logits, sorted_indices = torch.sort(logits, descending=True)
        probs = torch.softmax(sorted_logits, dim=-1)
        cumulative_probs = torch.cumsum(probs, dim=-1)

        # Remove tokens with cumulative probability above top_p
        sorted_indices_to_remove = cumulative_probs > top_p
        # shift the indices to include the first token above the threshold
        sorted_indices_to_remove[..., 1:] = sorted_indices_to_remove[..., :-1].clone()
        sorted_indices_to_remove[..., 0] = False

        indices_to_remove = sorted_indices[sorted_indices_to_remove]
        logits[indices_to_remove] = filter_value

    return logits


class AlliterativeGenerator:
    def __init__(self, model_name='flax-community/papuGaPT2'):
        self.utils = ModelUtils(model_name)
        self.tokenizer = self.utils.tokenizer
        self.model = self.utils.model
        self.device = self.utils.device
        self.prefix = []
        # State used during generation
        self.generated_words = []

    def generate_answer(self, prefix, target_char, max_new_tokens=50, top_k=50, top_p=0.9, temperature=1.0,
                      repetition_penalty=1.5, n_variants=8):
        # Generate several variants and pick the best according to scoring
        variants = []
        for i in tqdm(range(n_variants), desc="Generating variants", leave=False):
            text, logps = self.generate_single_variant(prefix, target_char,
                                                       max_new_tokens=max_new_tokens,
                                                       top_k=top_k, top_p=top_p,
                                                       temperature=temperature,
                                                       repetition_penalty=repetition_penalty)
            score = self.score_sequence(logps)
            variants.append((score, text, logps))

        # pick best
        variants.sort(key=lambda x: x[0], reverse=True)
        best = variants[0]
        return best[1]

    def generate_single_variant(self, prefix, target_char, max_new_tokens=50, top_k=50, top_p=0.9, temperature=1.0,
                                repetition_penalty=1.5):
        prompt = prefix
        remaining = max_new_tokens
        pbar = tqdm(total=max_new_tokens, desc="Generating words", leave=False)
        self.generated_words = []
        all_logps = []

        while remaining > 0:
            next_word, generated_tokens, logps = self.generate_one_word(prompt, target_char,
                                                                        top_k=top_k, top_p=top_p,
                                                                        temperature=temperature,
                                                                        repetition_penalty=repetition_penalty)
            if not next_word:
                break

            # punctuation/spacing handling: don't add a space before punctuation
            no_space_before = set([',', '.', '!', '?', ';', ':', ')', ']', '}', '”', '"', "'", '…'])
            first_char = next_word[0] if next_word else ''
            if prompt and (first_char in no_space_before or next_word in no_space_before):
                prompt += next_word
            else:
                prompt += ('' if prompt.endswith((" ", "\n", "(")) else ' ') + next_word

            if generated_tokens <= 0:
                break

            used = min(generated_tokens, remaining)
            pbar.update(used)
            remaining -= used

            # track generated words for repetition avoidance and scoring
            self.generated_words.append(re.sub(r"\s+", " ", next_word.strip()))
            all_logps.extend(logps)

            if next_word.endswith(('.', '!', '?')):
                break

        pbar.close()
        return prompt, all_logps

    def generate_one_word(self, prompt, target_char, max_tokens=10, top_k=50, top_p=0.9, temperature=1.0, repetition_penalty=1.5) -> tuple:
        # generate first token that starts with target_char
        first = self.generate_one_token(prompt, target_char, top_k=top_k, top_p=top_p,
                                        temperature=temperature, repetition_penalty=repetition_penalty)
        if not first:
            return ("", 0, [])
        first_tok, first_logp, first_id = first

        word = first_tok

        # prepare prompt for subsequent tokens
        prompt2 = prompt
        if prompt2 and not prompt2.endswith((" ", "\n")) and not first_tok.startswith((" ", "\n")):
            prompt2 += " "
        prompt2 += first_tok

        logps = [first_logp]
        generated_tokens = 1
        # keep appending continuation tokens until word boundary or punctuation
        while generated_tokens < max_tokens:
            nxt = self.generate_one_token(prompt2, None, top_k=top_k, top_p=top_p,
                                           temperature=temperature, repetition_penalty=repetition_penalty)
            if nxt is None:
                break
            next_tok, next_logp, next_id = nxt

            # stop on space/newline tokens
            if next_tok.startswith((" ", "\n")):
                break

            word += next_tok
            prompt2 += next_tok
            logps.append(next_logp)
            generated_tokens += 1

        return (word, generated_tokens, logps)

    def generate_one_token(self, prompt, target_char, temperature=1.0, top_k=50, top_p=0.9, repetition_penalty=1.5):
        device_local = self.utils.device

        # prep prompt
        inputs = self.utils.tokenizer(prompt, return_tensors='pt')
        input_ids = inputs['input_ids'].to(device_local)
        attention_mask = inputs.get('attention_mask', None)
        if attention_mask is not None:
            attention_mask = attention_mask.to(device_local)

        gen = []    # generated tokens
        with torch.no_grad():
            out = self.utils.model(input_ids=input_ids, attention_mask=attention_mask, use_cache=True)
            past = out.past_key_values
        last_token = input_ids[:, -1:].to(device_local)

        # calculate the tokens probability
        with torch.no_grad():
            out = self.utils.model(input_ids=last_token, past_key_values=past, use_cache=True)
            logits = out.logits[:, -1, :].squeeze(0)  # (vocab,)

        # prepare logits copy
        logits_proc = logits.clone()

        # apply repetition_penalty for tokens that lead to already generated words (only when starting a new word)
        if target_char is not None and self.generated_words:
            # build a set of lower-case generated words
            gen_words_set = set(w.lower() for w in self.generated_words)
        else:
            gen_words_set = set()

        # filter out tokens that do not start with target_char
        if target_char is not None:
            allowed = torch.zeros_like(logits_proc, dtype=torch.bool)
            for tid in range(logits_proc.size(0)):
                s = self.utils.tokenizer.decode([tid], clean_up_tokenization_spaces=False)
                s_stripped = s.lstrip()
                if not s_stripped:
                    continue
                ch = s_stripped[0]
                if ch.lower() == target_char.lower():
                    allowed[tid] = True
            logits_proc[~allowed] = -1e9

        # apply repetition penalty
        if gen_words_set:
            for tid in range(logits_proc.size(0)):
                try:
                    s = self.utils.tokenizer.decode([tid], clean_up_tokenization_spaces=False)
                except Exception:
                    continue
                token_str = s.strip().lower()
                # exact match with previous word (simplified heuristic)
                if token_str in gen_words_set:
                    logits_proc[tid] /= max(1.0, repetition_penalty)

        temp = (temperature if temperature > 0 else 1.0)
        logits_proc = logits_proc / temp

        # apply top-k and top-p
        filtered = top_k_top_p_filtering(logits_proc, top_k=top_k, top_p=top_p)

        probs = torch.softmax(filtered, dim=-1)

        # if all probs are zero or nan because of filtering, back off to original logits
        if torch.isfinite(probs).sum() == 0:
            probs = torch.softmax(logits / temp, dim=-1)

        next_token = torch.multinomial(probs, num_samples=1)
        next_id = next_token.item()

        if next_id == self.utils.tokenizer.eos_token_id:
            return None

        next_id = int(next_id)
        logp = float(torch.log(probs[next_id] + 1e-12))
        decoded = self.utils.tokenizer.decode([next_id], skip_special_tokens=True, clean_up_tokenization_spaces=False)
        return decoded, logp, next_id

    def score_sequence(self, logps, drop_penalty=1.0):
        """Score a sequence of token log-probabilities.
        Higher average log-probability is better; large sudden drops are penalized.
        """
        if not logps:
            return -1e9
        avg = sum(logps) / len(logps)
        drops = []
        for i in range(1, len(logps)):
            delta = logps[i-1] - logps[i]
            if delta > 0:
                drops.append(delta)
        avg_drop = sum(drops) / len(drops) if drops else 0.0
        score = avg - drop_penalty * avg_drop
        # small penalty for extremely low single-token prob
        min_lp = min(logps)
        if min_lp < -10:
            score -= 0.5 * (-10 - min_lp)
        return score


if __name__ == '__main__':
    gen = AlliterativeGenerator()

    prefixes = []
    with open('prefiksy.txt', 'r', encoding='utf-8') as f:
        prefixes = [ln.strip() for ln in f if ln.strip()]


    prefix = random.choice(prefixes) if prefixes else "Prawdziwy piekarz przyprawia pieczywo pieprzem"
    generation = gen.generate_answer(prefix, prefix[0].lower(), max_new_tokens=20, top_k=20, top_p=0.9, temperature=1.0)
    print(generation)
