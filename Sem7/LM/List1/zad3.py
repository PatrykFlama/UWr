import torch
from transformers import AutoTokenizer, AutoModelForCausalLM
from torch.nn import functional as F
import random
from tqdm.auto import tqdm

model_name = 'flax-community/papuGaPT2'
device = 'cuda'
device = 'cpu'

tokenizer = AutoTokenizer.from_pretrained(model_name)
model = AutoModelForCausalLM.from_pretrained(model_name).to(device)

def log_probs_from_logits(logits, labels):
    logp = F.log_softmax(logits, dim=-1)
    logp_label = torch.gather(logp, 2, labels.unsqueeze(2)).squeeze(-1)
    return logp_label
    
            
def sentence_prob(sentence_txt):
    input_ids = tokenizer(sentence_txt, return_tensors='pt')['input_ids'].to(device)
    with torch.no_grad():
        output = model(input_ids=input_ids)
        log_probs = log_probs_from_logits(output.logits[:, :-1, :], input_ids[:, 1:])
        seq_log_probs = torch.sum(log_probs)
    return seq_log_probs.cpu().numpy()  


methods = range(1, 8)
def construct_sentences(sentence, method):
    if method == 1:
        return (
            f"{sentence} Polecam!",
            f"{sentence} Nie polecam!",
        )
    elif method == 2:
        return (
            f"Ta opinia jest pozytywna: {sentence}",
            f"Ta opinia jest negatywna: {sentence}",
        )
    elif method == 3:
        return (
            f"To była opinia pozytywna: {sentence}",
            f"To była opinia negatywna: {sentence}",
        )
    elif method == 4:
        return (
            f"To jest opinia pozytywna: {sentence}",
            f"To jest opinia negatywna: {sentence}",
        )
    elif method == 5:
        return (
            f"{sentence} Ale super! Polecam każdemu! Fantastycznie!",
            f"{sentence} Ale kiepsko! Nie polecam nikomu! Okropnie!"
        )
    elif method == 6:
        return (
            f"{sentence} To była świetna decyzja! Zdecydowanie polecam!",
            f"{sentence} To była zła decyzja! Zdecydowanie nie polecam!",
        )
    elif method == 7:
        return (
            f"Polecam! Polecam! Polecam!\n{sentence}\nPolecam! Polecam! Polecam!",
            f"Nie polecam! Nie polecam! Nie polecam!\n{sentence}\nNie polecam! Nie polecam! Nie polecam!",
        )
    return (sentence, sentence)


def is_sentence_positive(sentence, method):
    sentence1, sentence2 = construct_sentences(sentence, method)
    prob1 = sentence_prob(sentence1)
    prob2 = sentence_prob(sentence2)
    if prob1 > prob2:
        return True
    else:
        return False


positive_sentences = [
    "Parking monitorowany w cenie.",
    "Hotel czysty, pokoje były sprzątane bardzo dokłądnie.",
    "Generalnie mogę go polecić, kierował mnie na potrzebne badania, analizował ich wyniki,",
    "cierpliwie odpowiadał na pytania.",
    "Fajny klimat pofabrykanckich kamienic.",
    "Sala zabaw dla dzieci, plac zabaw na zewnątrz, kominek, tenis stołowy.",
]

negative_sentences = [
    "W wielu pokojach niedziałająca klimatyzacja.",
    "Jedzenie mimo rzekomych dni europejskich monotonne.",
    "Drożej niż u konkurencji w podobnym standardzie.",
    "Może za szybko zrezygnowałam, ale szkoda mi było wydawać pieniędzy na spotkania,",
    "które nie przynosiły efektu.",
    "Omijaj to miejsce!",
]

sentences = [(sentence, True) for sentence in positive_sentences] + [(sentence, False) for sentence in negative_sentences]

methods = [4]
USE_FILE = True
DEBUG = False
accuracy_methods = []

if USE_FILE:
    with open("reviews_for_task3.txt", "r", encoding="utf-8") as f:
        lines = f.readlines()
    sentences = []
    for line in lines:
        line = line.strip()
        if line.startswith("GOOD"):
            sentences.append((line[6:].strip(), True))
        elif line.startswith("BAD"):
            sentences.append((line[5:].strip(), False))

if DEBUG == False:
    methods = tqdm(methods, desc="Methods")
    sentences_it = tqdm(sentences, desc="Sentences", leave=False)
else:
    sentences_it = sentences

for method in methods:
    correct = 0
    for sentence, is_positive in sentences_it:
        prediction = is_sentence_positive(sentence, method)

        if prediction == is_positive:
            correct += 1

        if DEBUG:
            print(f"Method {method} | Sentence: {sentence} | Predicted: {prediction} | Actual: {is_positive}")

    accuracy = correct / len(sentences)
    accuracy_methods.append((method, accuracy))
    
for method, accuracy in accuracy_methods:
    print(f"Method {method} Accuracy: {accuracy:.2f}")
