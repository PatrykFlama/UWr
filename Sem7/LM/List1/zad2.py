from transformers import pipeline, set_seed
import itertools
import re
from tqdm.auto import tqdm

MAX_TOKENS = 50
METHOD = 'prob_loss'  # 'chat' or 'log_prob'

generator = pipeline('text-generation', model='flax-community/papuGaPT2', device=0)

tokenizer = generator.tokenizer
model = generator.model

print ('Model loaded')

def score_sentence(sentence):
    words = ["tak", "nie", "natural", "nienatural"]
    scores = [1.0, -1.0, 0.5, -0.5]
    occurrences = [len(re.findall(r'\b' + re.escape(word) + r'\b', sentence)) for word in words]

    score = sum(o * s for o, s in zip(occurrences, scores)) / (sum(occurrences) + 1e-6)
    return score

def prob_score(sentence):
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

    return score_sentence(generation.lower()), generation

def log_prob(prompt):
    inputs = tokenizer(prompt, return_tensors="pt") # https://huggingface.co/docs/transformers/en/main_classes/tokenizer
    outputs = model(**inputs, labels=inputs["input_ids"])   # https://huggingface.co/docs/transformers/en/main_classes/model
    return -outputs.loss.item()



print("Enter a sentence: ")
user_sentence = input().strip(".\t\n").lower().split(" ")
permutations = list(itertools.permutations(user_sentence))

scored = []

for p in tqdm(permutations):
    sentence = " ".join(p) + ".\n"
    sentence = sentence[0].upper() + sentence[1:]

    score = 0
    if METHOD == 'chat':
        score, response = prob_score(sentence)
        print(f"Score: {score}\nSentence: {sentence}\n")
    elif METHOD == 'prob_loss':
        score = log_prob(sentence)

    scored.append((score, p))

scored.sort(reverse=True)

for score, sent in scored[:10]:
    print(f"Score: {score:<.2f}, Sentence: {' '.join(sent)}")


