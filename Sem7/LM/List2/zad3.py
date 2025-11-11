import torch
from transformers import AutoTokenizer, AutoModelForCausalLM
from torch.nn import functional as F
import re
import random
from tqdm.auto import tqdm
import equation

# # === Model ===
# # model_name = 'eryk-mazus/polka-1.1b-chat'
# model_name = 'flax-community/papuGaPT2'
# device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
# tokenizer = AutoTokenizer.from_pretrained(model_name)
# # ensure a pad token is set (avoid the warnings) and propagate to model config
# if tokenizer.pad_token is None:
#     tokenizer.pad_token = tokenizer.eos_token
# model = AutoModelForCausalLM.from_pretrained(model_name)
# model.config.pad_token_id = tokenizer.pad_token_id
# model.to(device)  # type: ignore
# print("Model loaded on", device)

# # === helpers ===
# def log_probs_from_logits(logits, labels):
#     logp = F.log_softmax(logits, dim=-1)
#     logp_label = torch.gather(logp, 2, labels.unsqueeze(2)).squeeze(-1)
#     return logp_label

# def sentence_prob(sentence_txt):
#     inputs = tokenizer(sentence_txt, return_tensors='pt', return_attention_mask=True)
#     input_ids = inputs['input_ids'].to(device)
#     attention_mask = inputs['attention_mask'].to(device)
#     with torch.no_grad():
#         output = model(input_ids=input_ids, attention_mask=attention_mask)
#         log_probs = log_probs_from_logits(output.logits[:, :-1, :], input_ids[:, 1:])
#         seq_log_probs = torch.sum(log_probs)
#     return seq_log_probs.cpu().numpy()  

# def ask_model(prompt, max_new_tokens=50, temperature=0.7):
#     inputs = tokenizer(prompt, return_tensors='pt', return_attention_mask=True)
#     input_ids = inputs['input_ids'].to(device)
#     attention_mask = inputs['attention_mask'].to(device)
#     with torch.no_grad():
#         output_ids = model.generate(
#             input_ids,
#             attention_mask=attention_mask,
#             max_new_tokens=max_new_tokens,
#             temperature=temperature,
#             do_sample=True,
#             pad_token_id=tokenizer.pad_token_id
#         )
#     output_text = tokenizer.decode(output_ids[0], skip_special_tokens=True)
#     return output_text


# ===== MAIN =====

def sentence_prob(_):
    return random.random()

def solve_iter_over_words(words: list[list[str]], k=5):
    best_k: list[tuple[float, list[str]]] = []

    for _ in range(k):
        sentence = []
        for word in words:
            sentence.append(random.choice(word))
        best_k.append((sentence_prob(' '.join(sentence)), sentence))


    for word_ptr in tqdm(range(len(words)), desc="Trying sentence prefix"):
        new_best_k = best_k.copy()
        for sample_prob, sample in tqdm(best_k, desc=f"Trying best_k for {word_ptr}", leave=False):
            for new_word_ptr in range(len(words[word_ptr])):
                new_sample = sample.copy()
                new_sample[word_ptr] = words[word_ptr][new_word_ptr]
                new_best_k.append((sentence_prob(' '.join(new_sample)), new_sample))
        best_k = new_best_k

        best_k.sort(reverse=True)
        best_k = best_k[:k]

    return best_k[0]


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
    best_sentence = solve_iter_over_words(words, 5)
    print(' '.join(best_sentence[1]))










