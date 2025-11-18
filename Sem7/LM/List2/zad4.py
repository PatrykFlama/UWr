import torch
import random
import math
import re
from tqdm.auto import tqdm
from model_lib import ModelUtils


def top_k_top_p_filtering(logits, top_k=0, top_p=0.0, filter_value=-1e9):
    # logits: 1D tensor
    top_k = min(top_k, logits.size(-1))

    if top_k > 0:
        # remove all tokens with a probability less than the top-k tokens
        indices_to_remove = logits < torch.topk(logits, top_k)[0][..., -1, None]
        logits[indices_to_remove] = filter_value

    if top_p > 0.0:
        sorted_logits, sorted_indices = torch.sort(logits, descending=True)
        cumulative_probs = torch.cumsum(torch.softmax(sorted_logits, dim=-1), dim=-1)

        # remove tokens with cumulative probability above the threshold
        sorted_indices_to_remove = cumulative_probs > top_p

        # shift the indices to the right to keep also the first token above the threshold
        sorted_indices_to_remove[..., 1:] = sorted_indices_to_remove[..., :-1].clone()
        sorted_indices_to_remove[..., 0] = 0

        indices_to_remove = sorted_indices[sorted_indices_to_remove]
        logits[indices_to_remove] = filter_value

    return logits


class AlliterativeGenerator:
    def __init__(self, model_name='flax-community/papuGaPT2', prefix_path=None):
        self.utils = ModelUtils(model_name)
        self.tokenizer = self.utils.tokenizer
        self.model = self.utils.model
        self.device = self.utils.device
        self.prefix = []
        if prefix_path is None:
            prefix_path = 'prefiksy.txt'
        try:
            with open(prefix_path, 'r', encoding='utf-8') as f:
                self.prefix = [ln.strip() for ln in f if ln.strip()]
        except Exception:
            self.prefix = []

    # def generate(self, prefix=None, max_new_tokens=50, top_k=50, top_p=0.9, temperature=1.0,
    #              n_variants=8, repetition_penalty=1.5):
    #     if prefix is None:
    #         prefix = random.choice(self.prefix)

    #     # determine target letter: first alphabetic character in prefix
    #     m = re.search(r"[A-Za-zÀ-ÖØ-öø-ÿ]", prefix)
    #     if not m:
    #         target_letter = 'a'
    #     else:
    #         target_letter = m.group(0).lower()

    #     variants = []

    #     for _ in tqdm(range(n_variants), desc="Generating variants", leave=False):
    #         txt, logps, words = self.generate_answer(prefix, target_letter,
    #                                               max_new_tokens=max_new_tokens,
    #                                               top_k=top_k, top_p=top_p,
    #                                               temperature=temperature,
    #                                               repetition_penalty=repetition_penalty)
    #         score = self._score_candidate(logps, words)
    #         variants.append((score, txt, logps, words))

    #     variants.sort(key=lambda x: x[0], reverse=True)
    #     best = variants[0] if variants else (None, '', [], [])
    #     return best, variants

    def generate_answer(self, prefix, target_char, max_new_tokens=50, top_k=50, top_p=0.9, temperature=1.0,
                      repetition_penalty=1.5):
        prompt = prefix
        remaining = max_new_tokens
        pbar = tqdm(total=max_new_tokens, desc="Generating words", leave=False)
        while remaining > 0:
            next_word, generated_tokens = self.generate_one_word(prompt, target_char,
                                          top_k=top_k, top_p=top_p,
                                          temperature=temperature)
            if not next_word:
                break

            prompt += next_word
            if generated_tokens <= 0:
                break
            used = min(generated_tokens, remaining)
            pbar.update(used)
            remaining -= used

        pbar.close()    
        return prompt

    def generate_one_word(self, prompt, target_char, max_tokens=10, top_k=50, top_p=0.9, temperature=1.0) -> tuple[str, int]:
        # generate first token that starts with target_char
        first_tok = self.generate_one_token(prompt, target_char, temperature=temperature)
        if not first_tok:
            return ("", 0)
        # first_tok = first_tok.lstrip()
        word = first_tok

        # prepare prompt for subsequent tokens
        prompt2 = prompt
        if prompt2 and not prompt2.endswith((" ", "\n")) and not first_tok.startswith((" ", "\n")):
            prompt2 += " "
        prompt2 += first_tok

        # keep appending continuation tokens until word boundary or punctuation
        generated_tokens = 1
        while generated_tokens < max_tokens:
            next_tok = self.generate_one_token(prompt2, None, temperature=temperature)
            if not next_tok:
                break
            # stop if token is pure punctuation or starts with white space
            if re.fullmatch(r"[.,;:?!-]+", next_tok) or next_tok.startswith((" ", "\n")):
                break

            # allow tokens that look like word continuations (letters, digits, apostrophe, hyphen, underscore)
            if re.match(r"^[\wÀ-ÖØ-öø-ÿ'-]", next_tok, re.UNICODE):
                word += next_tok
                if prompt2 and not prompt2.endswith((" ", "\n")) and not next_tok.startswith((" ", "\n")):
                    prompt2 += " "
                prompt2 += next_tok
                generated_tokens += 1
                continue
            break

        return (word, generated_tokens)

    def generate_one_token(self, prompt, target_char, temperature=1.0):
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
            logits = out.logits[:, -1, :]  # (1, vocab)
            past = out.past_key_values

        mask = torch.full_like(logits, -1e9)
        temp = (temperature if temperature > 0 else 1.0)

        if target_char != None:
            for tid in range(logits.size(0)):
                s = self.utils.tokenizer.decode([tid], clean_up_tokenization_spaces=False)
                s_stripped = s.lstrip()
                if not s_stripped:
                    continue
                ch = s_stripped[0]
                if ch.lower() == target_char.lower():
                    mask[0, tid] = logits[0, tid] / temp
        else:
            for tid in range(logits.size(0)):
                mask[0, tid] = logits[0, tid] / temp

        probs = torch.softmax(mask, dim=-1)

        next_token = torch.multinomial(probs, num_samples=1)  # shape (1,1)
        next_id = next_token.item()
        gen.append(next_id)
        last_token = next_token  # shape (1,1)

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
    gen = AlliterativeGenerator(prefix_path='prefixy.txt')

    # print('Loaded', len(gen.prefix), 'prefixes')
    # # generate a few examples
    # best, variants = gen.generate(n_variants=6, max_new_tokens=60, top_k=80, top_p=0.92, temperature=0.8)
    # print('\nBest candidate (score {:.4f}):\n'.format(best[0]) if best[0] is not None else '\nNo candidate\n')
    # for s in variants[:3]:
    #     sc, txt, logps, words = s
    #     print('SCORE=', sc)
    #     print(txt)
    #     print('WORDS:', words)
    #     print('---')

    generation = gen.generate_answer("Prawdziwy piekarz przyprawia pieczywo pieprzem", 'p', max_new_tokens=50, top_k=50, top_p=0.9, temperature=1.0)
    print(generation)
