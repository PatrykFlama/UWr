import torch
import random
import math
import re
from tqdm.auto import tqdm
from model_lib import ModelUtils


class AlliterativeGenerator:
    def __init__(self, model_name='flax-community/papuGaPT2'):
        self.utils = ModelUtils(model_name)
        self.tokenizer = self.utils.tokenizer
        self.model = self.utils.model
        self.device = self.utils.device
        self.prefix = []

    def generate_answer(self, prefix, target_char, max_new_tokens=50, top_k=50, top_p=0.9, temperature=1.0,
                      repetition_penalty=1.5):
        prompt = prefix
        remaining = max_new_tokens
        pbar = tqdm(total=max_new_tokens, desc="Generating words", leave=False)
        # keep track of words generated in this answer to avoid repetitions
        seen_words = set()
        # number of attempts to try before accepting a repeat; scale with repetition_penalty
        max_retries = max(1, int(repetition_penalty * 3))

        while remaining > 0:
            # ask generate_one_word to avoid words in seen_words; it will try several candidates
            next_word, generated_tokens = self.generate_one_word(prompt, target_char,
                                              max_tokens=10,
                                              top_k=top_k, top_p=top_p,
                                              temperature=temperature,
                                              excluded_words=seen_words,
                                              max_candidate_attempts=max_retries)

            if not next_word:
                break

            # if token ends with punctuation we accept and end generation
            if next_word.endswith(('.', '!', '?')):
                prompt += ('' if prompt.endswith((" ", "\n")) else ' ') + next_word
                break

            # append accepted word and record it
            prompt += ('' if prompt.endswith((" ", "\n")) else ' ') + next_word
            if generated_tokens <= 0:
                break

            used = min(generated_tokens, remaining)
            pbar.update(used)
            remaining -= used

            seen_words.add(next_word.strip().lower())

        pbar.close()    
        return prompt

    def generate_one_word(self, prompt, target_char, max_tokens=10, top_k=50, top_p=0.9, temperature=1.0,
                          excluded_words=None, max_candidate_attempts=3) -> tuple[str, int]:
        """Generate a single word (possibly multi-token). Try up to max_candidate_attempts
        candidates and return the first one not present in excluded_words. If none found,
        return the last candidate generated.
        """
        excluded = set(w.lower() for w in (excluded_words or []))
        last_candidate = ("", 0)

        for _ in range(max_candidate_attempts):
            # generate first token that starts with target_char
            first_tok = self.generate_one_token(prompt, target_char, temperature=temperature, top_k=top_k, top_p=top_p)
            if not first_tok:
                continue

            word = first_tok

            # prepare prompt for subsequent tokens
            prompt2 = prompt
            if prompt2 and not prompt2.endswith((" ", "\n")) and not first_tok.startswith((" ", "\n")):
                prompt2 += " "
            prompt2 += first_tok

            # keep appending continuation tokens until word boundary or punctuation
            generated_tokens = 1
            while generated_tokens < max_tokens:
                next_tok = self.generate_one_token(prompt2, None, temperature=temperature, top_k=top_k, top_p=top_p)

                if next_tok is None or next_tok.startswith((" ", "\n")):
                    break

                word += next_tok
                prompt2 += next_tok
                generated_tokens += 1

            last_candidate = (word, generated_tokens)
            lw = word.strip().lower()
            if lw and lw not in excluded:
                return (word, generated_tokens)

        return last_candidate

    def generate_one_token(self, prompt, target_char, temperature=1.0, top_k=50, top_p=0.9):
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

        # calculate the tokens probability for the next position
        with torch.no_grad():
            out = self.utils.model(input_ids=last_token, past_key_values=past, use_cache=True)
            logits = out.logits[:, -1, :]  # (1, vocab)

        # apply temperature
        temp = (temperature if temperature > 0 else 1.0)
        scores = logits / temp

        vocab_size = scores.size(-1)

        # if target_char specified, zero-out bad tokens
        if target_char is not None:
            for tid in range(vocab_size):
                s = self.utils.tokenizer.decode([tid], clean_up_tokenization_spaces=False)
                s_stripped = s.lstrip()
                if not s_stripped:
                    scores[0, tid] = -1e9
                    continue
                ch = s_stripped[0]
                if ch.lower() != target_char.lower():
                    scores[0, tid] = -1e9

        # convert to probabilities
        probs = torch.softmax(scores, dim=-1)

        # apply top-k filtering
        if top_k is not None and top_k > 0 and top_k < vocab_size:
            topk_vals, topk_idx = torch.topk(probs, top_k, dim=-1)
            mask = torch.zeros_like(probs)
            mask[0, topk_idx[0]] = probs[0, topk_idx[0]]
            probs = mask

        # apply top-p filtering
        if top_p is not None and 0.0 < top_p < 1.0:
            sorted_probs, sorted_idx = torch.sort(probs, descending=True)
            cumulative = torch.cumsum(sorted_probs, dim=-1)
            cutoff = (cumulative >= top_p).nonzero(as_tuple=False)
            if cutoff.numel() > 0:
                cutoff_idx = int(cutoff[0, 1])
                keep_idx = sorted_idx[0, :cutoff_idx + 1]
                mask = torch.zeros_like(probs)
                mask[0, keep_idx] = probs[0, keep_idx]
                probs = mask

        total = probs.sum()
        if total <= 0:
            # no valid tokens after filtering
            return None

        probs = probs / total

        next_token = torch.multinomial(probs, num_samples=1)  # shape (1,1)
        next_id = next_token.item()
        gen.append(next_id)

        if next_id == self.utils.tokenizer.eos_token_id:
            return None

        return self.utils.tokenizer.decode(gen, skip_special_tokens=True)


    def _score_candidate(self, logps, words):
        if not logps:
            return -1e9
        avg_lp = sum(logps) / len(logps)
        # repetition penalty: if any repeated word, reduce score
        rep_count = 0
        seen = set()
        for w in words:
            wl = w.lower()
            if wl in seen:
                rep_count += 1
            else:
                seen.add(wl)
        rep_pen = rep_count * 0.5

        # sudden drop penalty: look at consecutive logprob drops
        drops = [logps[i] - logps[i+1] for i in range(len(logps)-1)] if len(logps) > 1 else [0]
        max_drop = min(drops) if drops else 0.0
        drop_pen = 0.0
        if max_drop < -2.0:
            drop_pen = abs(max_drop) * 0.3

        score = avg_lp - rep_pen - drop_pen
        return score


if __name__ == '__main__':
    gen = AlliterativeGenerator()

    prefixes = []
    with open('prefiksy.txt', 'r', encoding='utf-8') as f:
        prefixes = [ln.strip() for ln in f if ln.strip()]


    prefix = random.choice(prefixes) if prefixes else "Prawdziwy piekarz przyprawia pieczywo pieprzem"
    print(prefix)
    generation = gen.generate_answer(prefix, prefix[0].lower(), max_new_tokens=20, top_k=50, top_p=0.9, temperature=1.0)
    print(generation)
