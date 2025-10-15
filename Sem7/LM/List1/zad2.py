from transformers import pipeline, set_seed
from transformers import AutoTokenizer, AutoModelForCausalLM
import itertools
import re
from tqdm.auto import tqdm
import torch
from torch.nn import functional as F
import random

MAX_TOKENS = 50
METHOD = 'sentence_prob'  # 'chat' or 'log_prob' or 'sentence_prob'

generator = pipeline('text-generation', model='flax-community/papuGaPT2', device=0)

tokenizer = generator.tokenizer
model = generator.model


print ('Model loaded')

def appr1_score_sentence(sentence):
    words = ["tak", "nie", "natural", "nienatural"]
    scores = [1.0, -1.0, 0.5, -0.5]
    occurrences = [len(re.findall(r'\b' + re.escape(word) + r'\b', sentence)) for word in words]

    score = sum(o * s for o, s in zip(occurrences, scores)) / (sum(occurrences) + 1e-6)
    return score

def appr1_prob_score(sentence):
    prompt = f'''
        Oceń, czy poniższe zdanie brzmi naturalnie po polsku.\n
        Zdanie: \"{sentence}\"\n
        Odpowiedź (tak lub nie):
    '''

    generation = generator(
        prompt,
        pad_token_id=generator.tokenizer.eos_token_id,
        max_new_tokens=MAX_TOKENS,
    )[0]['generated_text']

    return appr1_score_sentence(generation.lower()), generation


def appr2_log_prob(prompt):
    inputs = tokenizer(prompt, return_tensors="pt") # https://huggingface.co/docs/transformers/en/main_classes/tokenizer
    outputs = model(**inputs, labels=inputs["input_ids"])   # https://huggingface.co/docs/transformers/en/main_classes/model
    return -outputs.loss.item()




model_name = 'flax-community/papuGaPT2'
device = 'cuda'
device = 'cpu'

tokenizer = AutoTokenizer.from_pretrained(model_name)
model = AutoModelForCausalLM.from_pretrained(model_name).to(device)

def appr3_log_probs_from_logits(logits, labels):
    logp = F.log_softmax(logits, dim=-1)
    logp_label = torch.gather(logp, 2, labels.unsqueeze(2)).squeeze(-1)
    return logp_label
    
            
def appr3_sentence_prob(sentence_txt):
    input_ids = tokenizer(sentence_txt, return_tensors='pt')['input_ids'].to(device)
    with torch.no_grad():
        output = model(input_ids=input_ids)
        log_probs = appr3_log_probs_from_logits(output.logits[:, :-1, :], input_ids[:, 1:])
        seq_log_probs = torch.sum(log_probs)
    return seq_log_probs.cpu().numpy()  


print("Enter a sentence: ")
user_sentence = input().strip(".\t\n").lower().split(" ")
permutations = list(itertools.permutations(user_sentence))

scored = []

for p in tqdm(permutations):
    sentence = " ".join(p) + ".\n"
    sentence = sentence[0].upper() + sentence[1:]

    score = 0
    if METHOD == 'chat':
        score, response = appr1_prob_score(sentence)
        print(f"Score: {score}\nSentence: {sentence}\n")
    elif METHOD == 'prob_loss':
        score = appr2_log_prob(sentence)
    elif METHOD == 'sentence_prob':
        score = appr3_sentence_prob(sentence)

    scored.append((score, p))

scored.sort(reverse=True)

for score, sent in scored[:5]:
    print(f"Score: {score:<.2f}, Sentence: {' '.join(sent)}")

print("...")

for score, sent in scored[-5:]:
    print(f"Score: {score:<.2f}, Sentence: {' '.join(sent)}")

