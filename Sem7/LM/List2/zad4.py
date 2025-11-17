import torch
from torch.nn import functional as F
import random
from tqdm.auto import tqdm
import math
import re
import collections
from model_lib import ModelUtils


class AlliterativeSentenceGenerator:
    def __init__(self, model_name='flax-community/papuGaPT2', device=None):
        self.mu = ModelUtils(model_name)
        self.device = self.mu.device
        self.tokenizer = self.mu.tokenizer
        self.model = self.mu.model

    # --- prefix utilities ---
    def _word_head(self, word):
        m = re.search(r"[A-Za-zĄĆĘŁŃÓŚŹŻąćęłńóśźż]", word)
        return m.group(0).lower() if m else None

    def load_alliterative_prefixes(self, path='prefiksy.txt'):
        prefs = []
        with open(path, 'r') as f:
            for line in f:
                s = line.strip()
                if not s:
                    continue
                words = [w for w in re.split(r"\s+", s) if w]
                heads = [self._word_head(w) for w in words if self._word_head(w) is not None]
                if not heads:
                    continue
                # all words that have a letter head should share same head
                if all(h == heads[0] for h in heads):
                    prefs.append((s, heads[0]))
        return prefs

    # --- token helpers ---
    def token_starts_new_word(self, token_str):
        # tokens that start with a space start a new word in GPT-style tokenizers
        return token_str.startswith(' ')

    def token_letter_if_new_word(self, token_id):
        s = self.tokenizer.decode([token_id], clean_up_tokenization_spaces=False)
        if self.token_starts_new_word(s):
            # strip leading space and find first alphabetic char
            rest = s.lstrip()
            m = re.match(r"([A-Za-zĄĆĘŁŃÓŚŹŻąćęłńóśźż])", rest)
            return m.group(1).lower() if m else None
        return None

    # top-k and top-p (nucleus) filtering
    def top_k_top_p_filtering(self, logits, top_k=0, top_p=1.0, filter_value=-1e9):
        # logits: 1D tensor
        top_k = min(max(int(top_k), 0), logits.size(-1))
        if top_k > 0:
            # remove all tokens with a probability less than the top-kth token
            indices_to_remove = logits < torch.topk(logits, top_k)[0][..., -1, None]
            logits[indices_to_remove] = filter_value

        if top_p < 1.0:
            sorted_logits, sorted_indices = torch.sort(logits, descending=True)
            cumulative_probs = torch.cumsum(torch.softmax(sorted_logits, dim=-1), dim=-1)
            # remove tokens with cumulative probability above top_p
            sorted_indices_to_remove = cumulative_probs > top_p
            # shift the mask to the right to keep the first token above the threshold
            sorted_indices_to_remove[..., 1:] = sorted_indices_to_remove[..., :-1].clone()
            sorted_indices_to_remove[..., 0] = False
            indices_to_remove = sorted_indices[sorted_indices_to_remove]
            logits[indices_to_remove] = filter_value

        return logits

    def generate_one_variant(self, prefix, target_letter, max_new_tokens=60, top_k=50, top_p=0.9, temperature=1.0):
        # Prepare inputs
        inputs = self.tokenizer(prefix, return_tensors='pt', add_special_tokens=False)
        input_ids = inputs['input_ids'].to(self.device)
        attention_mask = inputs.get('attention_mask', None)
        if attention_mask is not None:
            attention_mask = attention_mask.to(self.device)

        gen_ids = []
        log_probs = []
        word_history = []  # full words generated (as strings)

        # get past
        with torch.no_grad():
            out = self.model(input_ids=input_ids, attention_mask=attention_mask, use_cache=True)
            past = out.past_key_values
        last_token = input_ids[:, -1:]

        for step in tqdm(range(max_new_tokens), desc="Generating tokens", leave=False):
            with torch.no_grad():
                out = self.model(input_ids=last_token, past_key_values=past, use_cache=True)
                logits = out.logits[:, -1, :].clone()
                past = out.past_key_values

            logits = logits / (temperature if temperature > 0 else 1.0)

            # Modify logits to favor tokens that start a word with target_letter and to discourage undesirable starts
            vocab_size = logits.size(-1)
            logits_cpu = logits.squeeze(0).cpu()

            # apply character-based shaping
            for tid in range(vocab_size):
                token_letter = self.token_letter_if_new_word(tid)
                if token_letter is not None:
                    # this token starts a new word
                    if token_letter == target_letter:
                        # boost tokens that start with target letter
                        logits_cpu[tid] += 2.0
                    else:
                        # penalize tokens that start with other letters
                        logits_cpu[tid] -= 3.0
                else:
                    # token continues previous token: slightly prefer letter continuations over digits/punct
                    dec = self.tokenizer.decode([tid], clean_up_tokenization_spaces=False)
                    # if token begins with a digit or symbol and would start a word later, penalize slightly
                    if re.match(r"^[0-9]", dec.lstrip()):
                        logits_cpu[tid] -= 2.0

            logits = logits_cpu.to(self.device).unsqueeze(0)

            # avoid repeating the same word: if token starts a new word equal to last generated word, penalize
            if word_history:
                last_word = word_history[-1].lower()
                # check tokens that would immediately create the same word (first token of new word equals last_word)
                for tid in range(vocab_size):
                    token_str = self.tokenizer.decode([tid], clean_up_tokenization_spaces=False)
                    if token_str.startswith(' '):
                        stripped = token_str.lstrip()
                        if stripped:
                            candidate = stripped.split()[0]
                            if candidate.lower() == last_word:
                                logits[0, tid] -= 5.0

            # Apply top-k & top-p
            filtered_logits = self.top_k_top_p_filtering(logits[0].clone(), top_k=top_k, top_p=top_p)
            probs = torch.softmax(filtered_logits, dim=-1)

            # If all probs are zero (unlikely), fallback to raw softmax
            if torch.isfinite(probs).sum() == 0:
                probs = torch.softmax(logits, dim=-1).squeeze(0)

            next_token = torch.multinomial(probs, num_samples=1).to(self.device)
            next_id = next_token.item()
            # get log prob
            # ensure integer indexing and compute float log-prob
            lp = math.log(probs[int(next_id)].item() + 1e-12)
            log_probs.append(lp)

            gen_ids.append(next_id)
            last_token = next_token.unsqueeze(0)

            # update word history when token starts a new word
            token_str = self.tokenizer.decode([next_id], clean_up_tokenization_spaces=False)
            if token_str.startswith(' '):
                w = token_str.lstrip()
                # if token contains punctuation immediately, treat appropriately
                w_clean = re.match(r"([A-Za-zĄĆĘŁŃÓŚŹŻąćęłńóśźż]+)", w)
                if w_clean:
                    word_history.append(w_clean.group(1))
                else:
                    # punctuation or other: if sentence-ending punctuation, finish
                    if re.match(r"^[\.\?!]", w):
                        # finish on end punctuation
                        break
            else:
                # token continuing previous token: check if it contains sentence-ending punctuation
                if re.search(r"[\.\?!]", token_str):
                    break

            # stop if we generated an explicit sentence terminator token (safety)
            if len(gen_ids) >= max_new_tokens:
                break

        # build full text
        out_ids = torch.cat([input_ids.squeeze(0), torch.tensor(gen_ids, dtype=torch.long)], dim=0).tolist()
        text = self.tokenizer.decode(out_ids, skip_special_tokens=True)

        # postprocess spacing/punctuation: remove spaces before punctuation
        text = re.sub(r"\s+([\.,;:\?!])", r"\1", text)
        text = text.strip()
        # ensure ends with sentence punctuation
        if not re.search(r"[\.!?]$", text):
            text = text + '.'

        return {
            'text': text,
            'log_probs': log_probs,
            'word_history': word_history,
            'prefix': prefix
        }

    def score_variant(self, v):
        # score based on average log-prob and penalize sharp drops and repetition
        lps = v['log_probs']
        if not lps:
            return -1e9
        avg_lp = sum(lps) / len(lps)
        # penalize large negative drops between consecutive tokens
        drops = [max(0.0, lps[i-1] - lps[i]) for i in range(1, len(lps))]
        drop_penalty = sum(drops) / (len(drops) + 1)
        # repetition penalty: simple check of repeated words
        reps = 0
        wh = [w.lower() for w in v['word_history']]
        if wh:
            counter = collections.Counter(wh)
            for w, c in counter.items():
                if c > 1:
                    reps += (c - 1)
        rep_penalty = reps * 0.5

        score = avg_lp - drop_penalty - rep_penalty
        return score

    def generate(self, prefix, n_candidates=8, **gen_kwargs):
        # determine target letter from prefix (first alphabetic letter of first word)
        words = [w for w in re.split(r"\s+", prefix) if w]
        first_head = self._word_head(words[0]) if words else None
        if not first_head:
            raise ValueError('Prefix has no alphabetic start')

        candidates = []
        for _ in tqdm(range(n_candidates), desc="Generating candidates"):
            v = self.generate_one_variant(prefix, target_letter=first_head, **gen_kwargs)
            candidates.append(v)

        # score and pick best
        scored = [(self.score_variant(v), v) for v in candidates]
        scored.sort(key=lambda x: x[0], reverse=True)
        best_score, best = scored[0]
        return best, scored


if __name__ == '__main__':
    random.seed(42)
    gen = AlliterativeSentenceGenerator()
    prefs = gen.load_alliterative_prefixes('prefiksy.txt')
    if not prefs:
        print('No alliterative prefixes found in prefiksy.txt')
    else:
        # choose random prefix that is alliterative
        prefix, letter = random.choice(prefs)
        print('Chosen prefix:', prefix, 'letter=', letter)
        best, scored = gen.generate(prefix, n_candidates=6, max_new_tokens=60, top_k=60, top_p=0.9, temperature=0.8)
        print('\nBest generation (score={:.3f}):\n{}'.format(scored[0][0] if scored else 0.0, best['text']))
        print('\nAll candidates scores:')
        for s, v in scored:
            print(f"{s:.3f}\t{v['text']}")

