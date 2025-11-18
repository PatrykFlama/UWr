import random
import math
import torch
import numpy as np
from model_lib import ModelUtils

# Simple util: is punctuation considered sentence terminator
SENTENCE_END_TOKS = {'.', '!', '?'}
PUNCTUATION = {',', ';', ':', '(', ')', '"', "'", '—', '-', '…'}


class ModelAlliterativeGenerator:
    def __init__(self, model_name='flax-community/papuGaPT2', device=None):
        self.model_utils = ModelUtils(model_name)
        self.tokenizer = self.model_utils.tokenizer
        self.model = self.model_utils.model
        self.device = self.model_utils.device

        # Precompute token texts for faster filtering
        # prefer model's vocab size (logits size); fall back to tokenizer's vocab size
        self.vocab_size = getattr(self.model.config, 'vocab_size', None) or self.tokenizer.vocab_size
        # Some tokenizer implementations expose a slightly different vocab_size; ensure we use the model's output dim
        try:
            model_vocab = getattr(self.model.config, 'vocab_size', None)
            if model_vocab is not None:
                self.vocab_size = model_vocab
        except Exception:
            pass

        self._token_texts = [self.tokenizer.decode([i], clean_up_tokenization_spaces=False) for i in range(self.vocab_size)]

    def _top_k_top_p_filter(self, logits, top_k=50, top_p=0.9):
        # logits: 1D tensor
        logits = logits.clone()
        # top-k
        if top_k is not None and top_k > 0:
            topk_vals, _ = torch.topk(logits, min(top_k, logits.size(-1)))
            min_topk = topk_vals[-1]
            logits[logits < min_topk] = -1e9

        # top-p (nucleus)
        if top_p is not None and 0.0 < top_p < 1.0:
            sorted_logits, sorted_indices = torch.sort(logits, descending=True)
            probs = torch.softmax(sorted_logits, dim=-1)
            cumulative = torch.cumsum(probs, dim=-1)
            # remove tokens with cumulative prob above top_p
            sorted_indices_to_remove = cumulative > top_p
            # keep at least one
            if sorted_indices_to_remove[0]:
                sorted_indices_to_remove[0] = False
            indices_to_remove = sorted_indices[sorted_indices_to_remove]
            logits[indices_to_remove] = -1e9

        return logits

    def _allowed_mask_for_alliteration(self, required_letter, new_word_flag):
        # returns boolean mask (True = allowed)
        req = required_letter.lower() if required_letter is not None else None
        mask = torch.zeros(self.vocab_size, dtype=torch.bool)
        for i, t in enumerate(self._token_texts):
            txt = t
            # treat tokens that start with a space as starting a new word
            starts_new = txt.startswith(' ')
            core = txt.lstrip()
            if core == '':
                # whitespace-only token (rare) - allow only if not starting a new required word
                mask[i] = not new_word_flag
                continue

            # punctuation tokens (commas etc.) should generally be allowed (they attach to previous word)
            if all(ch in PUNCTUATION or ch in SENTENCE_END_TOKS for ch in core):
                # allow punctuation tokens regardless of new_word_flag
                mask[i] = True
                continue

            # if this token starts a new word, enforce required letter
            if starts_new and new_word_flag:
                first_char = core[0].lower()
                # allow only if alphabetic and matches required
                if req is None:
                    mask[i] = core[0].isalpha()
                else:
                    mask[i] = core[0].isalpha() and first_char == req
            else:
                # continuation or we don't enforce letter now: allow tokens that are letter/word pieces
                # we allow tokens that begin with letter or don't begin with a weird symbol
                mask[i] = True

        return mask

    def _penalize_repetition(self, logits, generated_words, candidate_texts):
        # candidate_texts: list of token texts for each vocab id
        logits = logits.clone()
        for i, txt in enumerate(candidate_texts):
            core = txt.lstrip()
            if core == '':
                continue
            # if this token begins a new word
            if txt.startswith(' '):
                word_start = core.split()[0].lower()
                if word_start in generated_words:
                    logits[i] -= 5.0  # strong penalty for repeating whole word
        return logits

    def generate_variant(self, prompt, required_letter=None, max_new_tokens=60, top_k=50, top_p=0.9, temperature=1.0):
        # Tokenize prompt
        inputs = self.tokenizer(prompt, return_tensors='pt')
        input_ids = inputs['input_ids'].to(self.device)
        attention_mask = inputs.get('attention_mask', None)
        if attention_mask is not None:
            attention_mask = attention_mask.to(self.device)

        # prepare past
        with torch.no_grad():
            out = self.model(input_ids=input_ids, attention_mask=attention_mask, use_cache=True)
            past = out.past_key_values
        last_token = input_ids[:, -1:].to(self.device)

        generated_ids = []
        generated_texts = []
        generated_words = set()
        # determine if next token is new word by inspecting last token decode
        last_decoded = self.tokenizer.decode(input_ids[0], clean_up_tokenization_spaces=False)
        new_word_flag = last_decoded.endswith(' ') or last_decoded == ''

        token_logprobs = []

        for step in range(max_new_tokens):
            with torch.no_grad():
                out = self.model(input_ids=last_token, past_key_values=past, use_cache=True)
                logits = out.logits[:, -1, :].squeeze(0).cpu()
                past = out.past_key_values

            # build allowed mask based on alliteration rule and punctuation rules
            allowed_mask = self._allowed_mask_for_alliteration(required_letter, new_word_flag)

            # mask out disallowed tokens
            logits_masked = logits.clone()
            logits_masked[~allowed_mask] = -1e9

            # apply repetition penalty using token texts
            logits_masked = self._penalize_repetition(logits_masked, generated_words, self._token_texts)

            # apply top-k and top-p
            logits_filtered = self._top_k_top_p_filter(logits_masked, top_k=top_k, top_p=top_p)

            # temperature
            temp = temperature if temperature > 0 else 1.0
            probs = torch.softmax(logits_filtered / temp, dim=-1)

            # sample
            next_id = torch.multinomial(probs, num_samples=1).item()
            next_txt = self._token_texts[next_id]
            # compute logprob for scoring
            lp = torch.log(probs[next_id].clamp(min=1e-12)).item()
            token_logprobs.append(lp)

            generated_ids.append(next_id)
            generated_texts.append(next_txt)

            # update generated_words when token starts new word
            if next_txt.startswith(' '):
                core = next_txt.lstrip()
                # get the first contiguous alphabetical/hyphen chunk (may be only subword)
                candidate = ''.join([c for c in core if c.isalpha() or c == "-"]).strip()
                if candidate:
                    first_word = candidate.split()[0]
                    if first_word:
                        generated_words.add(first_word.lower())

            # update new_word_flag for next token: next token starts new word if this token ends with a space
            new_word_flag = False
            # many tokens include leading spaces; to see whether next token will start a new word we check if current token ends with a space
            if next_txt.endswith(' '):
                new_word_flag = True

            last_token = torch.tensor([[next_id]], device=self.device)

            # stop if generated token ends the sentence
            core = next_txt.lstrip()
            if len(core) > 0 and core[0] in SENTENCE_END_TOKS:
                break

        # decode full generation
        decoded = self.tokenizer.decode(input_ids[0].tolist() + generated_ids, skip_special_tokens=True)
        return decoded, token_logprobs

    def score_variant(self, token_logprobs, generated_text):
        # Score: average logprob, penalize big drops and repetitions, prefer ending with sentence end
        if not token_logprobs:
            return -1e9
        arr = np.array(token_logprobs)
        avg = arr.mean()
        min_drop = arr.min() - avg  # large negative means drop
        # repetition penalty: approximate by counting repeated words
        words = [w for w in ''.join(generated_text).split() if w.isalpha()]
        rep_pen = len(words) - len(set(words))
        score = avg - 0.5 * abs(min_drop) - 0.5 * rep_pen
        # bonus if ends with sentence-ending punctuation
        if generated_text.strip() and generated_text.strip()[-1] in SENTENCE_END_TOKS:
            score += 0.5
        return score

    def generate(self, prefix, required_letter=None, n_variants=8, **gen_kwargs):
        # If no required_letter, infer from prefix if possible
        prefix_words = [w.strip(" ,.;:?!()\"'") for w in prefix.split() if w.strip()]
        if required_letter is None and prefix_words:
            # check if prefix itself is alliterative
            first_letters = [w[0].lower() for w in prefix_words if w]
            if all(fl == first_letters[0] for fl in first_letters):
                required_letter = first_letters[0]
            else:
                # fallback: set required_letter to first letter of first word
                required_letter = first_letters[0]

        variants = []
        for _ in range(n_variants):
            gen_text, token_logprobs = self.generate_variant(prefix, required_letter=required_letter, **gen_kwargs)
            variants.append((gen_text, token_logprobs))

        # pick best by score
        best = None
        best_score = -1e9
        for text, tlogp in variants:
            score = self.score_variant(tlogp, text)
            if score > best_score:
                best_score = score
                best = (text, score)

        return best, variants


def _load_alliterative_prefixes(path, min_words=2):
    # load prefixes that themselves are alliterative (every word starts with same letter)
    good = []
    with open(path, encoding='utf-8') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            words = [w.strip(" ,.;:?!()\"'") for w in line.split() if w.strip()]
            if len(words) < min_words:
                continue
            letters = [w[0].lower() for w in words if w]
            if letters and all(l == letters[0] for l in letters):
                good.append(line)
    return good


if __name__ == '__main__':
    import argparse
    import os

    parser = argparse.ArgumentParser()
    parser.add_argument('--prefix-file', default=os.path.join(os.path.dirname(__file__), 'prefiksy.txt'))
    parser.add_argument('--n', type=int, default=5)
    parser.add_argument('--top_k', type=int, default=50)
    parser.add_argument('--top_p', type=float, default=0.9)
    parser.add_argument('--temperature', type=float, default=0.8)
    args = parser.parse_args()

    prefixes = _load_alliterative_prefixes(args.prefix_file)
    if not prefixes:
        print('No strictly alliterative prefixes found; falling back to random prefixes')
        with open(args.prefix_file, encoding='utf-8') as f:
            allp = [l.strip() for l in f if l.strip()]
        prefixes = allp

    # pick a random prefix that is short enough
    prefix = random.choice(prefixes[:200])
    print('Prefix:', prefix)

    gen = ModelAlliterativeGenerator()
    (best_text, best_score), all_variants = gen.generate(prefix, n_variants=args.n, max_new_tokens=60, top_k=args.top_k, top_p=args.top_p, temperature=args.temperature)

    print('\nVariants:')
    for t, lp in all_variants:
        print('---')
        print(t)
    print('\nBest (score={:.3f}):'.format(best_score))
    print(best_text)
