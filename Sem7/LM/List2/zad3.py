import torch
from transformers import AutoTokenizer, AutoModelForCausalLM
from torch.nn import functional as F
import re
import random
from tqdm.auto import tqdm
import equation
from model_lib import ModelUtils

# ===== MAIN =====
utils = ModelUtils()

# def sentence_prob(s):
#     return utils.sentence_prob(s)

def sentence_prob(_):
    return random.random()

def solve_iter_over_words(words: list[list[str]], k=5, reiterations=5):
    best_k: list[tuple[float, list[str]]] = []

    for _ in range(k):
        sentence = []
        for word in words:
            sentence.append(random.choice(word))
        best_k.append((sentence_prob(' '.join(sentence)), sentence))

    for _ in tqdm(range(reiterations), desc="Reiterations", leave=False):
        for word_ptr in range(len(words)):
            new_best_k = best_k.copy()
            for sample_prob, sample in tqdm(best_k, desc=f"Trying best_k for {word_ptr}", leave=False):
                for new_word_ptr in range(len(words[word_ptr])):
                    new_sample = sample.copy()
                    new_sample[word_ptr] = words[word_ptr][new_word_ptr]
                    new_best_k.append((sentence_prob(' '.join(new_sample)), new_sample))
            best_k = new_best_k

            best_k.sort(reverse=True)
            best_k = best_k[:k]
            
    for _ in tqdm(range(reiterations), desc="Reiterations", leave=False):
        for sentence in best_k:
            # find word with smallest probability and try all variants
            # try all single-word variants of each sentence in current best_k
            current_best = best_k.copy()
            for sample_prob, sample in current_best:
                for word_idx in range(len(sample)):
                    for variant in words[word_idx]:
                        if variant == sample[word_idx]:
                            continue
                        new_sample = sample.copy()
                        new_sample[word_idx] = variant
                        best_k.append((sentence_prob(' '.join(new_sample)), new_sample))

        # keep top-k after expanding
        best_k.sort(reverse=True)
        best_k = best_k[:k]



    return best_k


def get_solution(words):
    res = ""
    for word in words:
        if len(res) > 0:
            res += ' '
        res += word[0]
    return res

if __name__ == "__main__":
    text = "wprost|wyprosty|wyprostu|wyprost uwielbiała|wielbił|wielbiła|uwielbił|wielbiło|uwielbiał|uwielbiało|uwielbiały\
 słuchać|osłuchać|słychać|usłuchać o|i|e|a|ó|ę|y|ą|u\
 wartościach własnych|owłosionych macierzy|mocarz|macierzą|macierze|mocarza|mocarze|mocarzy|macierz"

    words = [word.split('|') for word in text.split(' ')]

    print(get_solution(words))
    best_sentence = solve_iter_over_words(words, 15) # type: ignore
    # print(' '.join(best_sentence[1]))
    for score, sentence in best_sentence[:5]:
        print(f"{score}: {' '.join(sentence)}")









