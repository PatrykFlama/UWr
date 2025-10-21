import torch
from transformers import AutoTokenizer, AutoModelForCausalLM
from torch.nn import functional as F
import re
import random
from tqdm.auto import tqdm

QNA_SAMPLE = 0.1
SAVE_ANSWERS = True
SAVE_QUESTIONS = True

FEWSHOT = {
    "OSOBY": """Pytanie: Kto napisał Pana Tadeusza?
Odpowiedź: Adam Mickiewicz.
Pytanie: Kto był pierwszym prezydentem Polski?
Odpowiedź: Gabriel Narutowicz.
Pytanie: Kto był autorem książki Quo Vadis?
Odpowiedź: Henryk Sienkiewicz.
""",
    "MIEJSCA": """Pytanie: W jakim mieście znajduje się Wawel?
Odpowiedź: Kraków.
Pytanie: W jakim kraju leży Rzym?
Odpowiedź: Włochy.
""",
    "CYTATY": """Pytanie: Kto powiedział: "Być albo nie być?"
Odpowiedź: William Shakespeare.
Pytanie: Kto powiedział: "Miej serce i patrzaj w serce"?
Odpowiedź: Adam Mickiewicz.
""",
    "DEFINICJA": """Pytanie: Co to jest komputer?
Odpowiedź: Urządzenie elektroniczne do przetwarzania danych.
Pytanie: Co to jest stolica?
Odpowiedź: Główne miasto kraju.
"""
}

# === Model ===
model_name = 'eryk-mazus/polka-1.1b-chat'
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
tokenizer = AutoTokenizer.from_pretrained(model_name)
model = AutoModelForCausalLM.from_pretrained(model_name).to(device)
print("Model loaded on", device)

# === helpers ===
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


def ask_model(question, shots=None):
    base_prompt = ""
    if shots:
        base_prompt += shots + "\n\n"
    prompt = f"{base_prompt}Pytanie: {question}\nOdpowiedź:"
    inputs = tokenizer(prompt, return_tensors="pt").to(device)
    output = model.generate(
        **inputs,
        max_new_tokens=15,
        temperature=0.7,
        top_p=0.8,
        repetition_penalty=1.2,
        pad_token_id=tokenizer.eos_token_id,
    )
    text = tokenizer.decode(output[0], skip_special_tokens=True)
    ans = text.split("Odpowiedź:")[-1].strip().split('\n')[0]
    return ans

# === heuristics ===
def classify_question(q):
    q = q.lower().strip()

    if q.startswith("czy"):
        return "TAK_NIE"

    if "ile" in q or "którym roku" in q:
        return "LICZBA"

    if q.startswith("co") or "oznacza" in q:
        return "DEFINICJA"

    return "INNE"

def heuristic_answer(q):
    group = classify_question(q)

    # TAK/NIE
    if group == "TAK_NIE":
        candidates = ["Tak.", "Nie."]
        probs = [sentence_prob(f"{q} Odpowiedź: {c}") for c in candidates]
        return candidates[probs.index(max(probs))]

    # ILE / KTÓRY ROK
    if group == "LICZBA":
        ans = ask_model(q)
        nums = re.findall(r"\d+", ans)
        return nums[0] if nums else ans

    # CO TO JEST / CO OZNACZA
    if group == "DEFINICJA":
        few_shot = FEWSHOT[group]
        return ask_model(q, shots=few_shot)

    return ask_model(q)

# ====== MAIN ======
with open("task4_questions.txt", encoding="utf-8") as f:
    questions = [x.strip() for x in f if x.strip()]
    questions_idx = random.sample(range(len(questions)), int(len(questions) * QNA_SAMPLE))
    questions = [questions[i] for i in questions_idx]

if SAVE_ANSWERS:
    with open("task4_answers.txt", "r", encoding="utf-8") as f:
        real_answers = [x.strip() for x in f if x.strip()]
        real_answers = [real_answers[i] for i in questions_idx]

    with open("correct_answers.txt", "w", encoding="utf-8") as f:
        for a in real_answers:
            f.write(a + "\n")
        
if SAVE_QUESTIONS:
    with open("task4_questions_sampled.txt", "w", encoding="utf-8") as f:
        for q in questions:
            f.write(q + "\n")

answers = []
for q in tqdm(questions, desc="Odpowiadanie na pytania"):
    ans = heuristic_answer(q)
    answers.append(ans)

with open("found_answers.txt", "w", encoding="utf-8") as f:
    for a in answers:
        f.write(a.strip() + "\n")
