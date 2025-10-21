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

# Add a few-shot template for short factual Q/A and yes/no questions to improve
# model conditioning for factoid responses.
FEWSHOT_FACT = """Pytanie: Który stan USA ma największą powierzchnię?
Odpowiedź: Alaska.
Pytanie: Kto był autorem powieści "Ziemia obiecana"?
Odpowiedź: Władysław Reymont.
Pytanie: Czy Merkury jest większy od Ziemi?
Odpowiedź: Nie.
Pytanie: Ile jest 12 podzielić na 4?
Odpowiedź: 3.
"""

# === Model ===
model_name = 'eryk-mazus/polka-1.1b-chat'
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
tokenizer = AutoTokenizer.from_pretrained(model_name)
model = AutoModelForCausalLM.from_pretrained(model_name)
model.to(device)  # type: ignore
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
    # Build prompt with optional few-shot examples
    base_prompt = ""
    if shots:
        base_prompt = shots.strip() + "\n\n"
    prompt = f"{base_prompt}Pytanie: {question}\nOdpowiedź:"
    inputs = tokenizer(prompt, return_tensors="pt").to(device)

    # Use more deterministic generation for short, factual answers
    # when prompt looks like a factoid (few-shot provided) - lower temp, beams.
    # Generation parameters (explicit to satisfy static analysis)
    max_new_tokens = 20
    pad_token_id = tokenizer.eos_token_id
    do_sample = True
    temperature = 0.7
    top_p = 0.9
    repetition_penalty = 1.2
    num_beams = None

    # If shots supplied and seem factual, force deterministic decoding
    if shots and len(shots) > 0:
        do_sample = False
        num_beams = 5
        temperature = 0.0

    # Pass tokenized inputs as explicit tensors on the model device
    input_ids = inputs['input_ids'].to(device)
    attention_mask = inputs.get('attention_mask')
    if attention_mask is not None:
        attention_mask = attention_mask.to(device)

    # Call generate with explicit named params
    if num_beams is not None:
        output = model.generate(
            input_ids=input_ids,
            attention_mask=attention_mask,
            max_new_tokens=max_new_tokens,
            pad_token_id=pad_token_id,
            do_sample=do_sample,
            temperature=temperature,
            top_p=top_p,
            repetition_penalty=repetition_penalty,
            num_beams=num_beams,
        )
    else:
        output = model.generate(
            input_ids=input_ids,
            attention_mask=attention_mask,
            max_new_tokens=max_new_tokens,
            pad_token_id=pad_token_id,
            do_sample=do_sample,
            temperature=temperature,
            top_p=top_p,
            repetition_penalty=repetition_penalty,
        )
    text = tokenizer.decode(output[0], skip_special_tokens=True)
    ans = text.split("Odpowiedź:")[-1].strip().split('\n')[0]
    # basic cleaning
    ans = ans.strip()
    # remove leading punctuation/quotes
    ans = re.sub(r'^["\'"`\\s:.–—\-]+', '', ans)
    return ans

# === heuristics ===
def classify_question(q):
    q = q.lower().strip()
    # yes/no
    if q.startswith("czy") or q.startswith("czyżby"):
        return "TAK_NIE"

    # numeric questions
    if any(tok in q for tok in ["ile", "ile wynosi", "ile jest", "którym roku", "kiedy", "data"]):
        return "LICZBA"

    # definition-like
    if q.startswith("co") or "oznacza" in q or q.startswith("jak nazywa się"):
        return "DEFINICJA"

    # person/place detection heuristics
    if q.startswith("kto") or "który" in q and any(w in q for w in ["napisał", "był", "autor", "dowódca", "prezydent", "kto"]):
        return "OSOBA"
    if any(w in q for w in ["miasto", "kraj", "województwie", "gdzie", "stan", "wyspa", "rzeka", "na terenie"]):
        return "MIEJSCE"

    return "INNE"

def heuristic_answer(q):
    group = classify_question(q)

    # TAK/NIE
    if group == "TAK_NIE":
        # Use a short few-shot and score the two options using sentence probability
        few = FEWSHOT_FACT
        yes = "Tak."
        no = "Nie."
        # Score full prompt + candidate answer
        p_yes = sentence_prob(f"{few}\nPytanie: {q} Odpowiedź: {yes}")
        p_no = sentence_prob(f"{few}\nPytanie: {q} Odpowiedź: {no}")
        return yes if p_yes >= p_no else no

    # ILE / KTÓRY ROK
    if group == "LICZBA":
        # Ask with factual few-shot and prefer extracted numbers
        ans = ask_model(q, shots=FEWSHOT_FACT)
        nums = re.findall(r"\d+", ans)
        if nums:
            return nums[0]
        # also try to parse simple written numbers (one, dwa, trzy) - keep minimal
        word_to_digit = {"jeden": "1", "dwa": "2", "trzy": "3", "cztery": "4", "pięć": "5", "sześć": "6"}
        for w, d in word_to_digit.items():
            if re.search(rf"\b{w}\b", ans.lower()):
                return d
        return ans

    # CO TO JEST / CO OZNACZA
    if group == "DEFINICJA":
        few_shot = FEWSHOT.get(group, "")
        return ask_model(q, shots=few_shot)

    # For person/place try targeted few-shot to improve accuracy
    if group == "OSOBA":
        return ask_model(q, shots=FEWSHOT.get("OSOBY", ""))
    if group == "MIEJSCE":
        return ask_model(q, shots=FEWSHOT.get("MIEJSCA", ""))

    # fallback: use general factual few-shot to bias concise answers
    return ask_model(q, shots=FEWSHOT_FACT)

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
