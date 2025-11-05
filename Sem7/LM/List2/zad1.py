import torch
from transformers import AutoTokenizer, AutoModelForCausalLM
from torch.nn import functional as F
import re
import random
from tqdm.auto import tqdm
import equation

# === Model ===
# model_name = 'eryk-mazus/polka-1.1b-chat'
model_name = 'flax-community/papuGaPT2'
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
tokenizer = AutoTokenizer.from_pretrained(model_name)
# ensure a pad token is set (avoid the warnings) and propagate to model config
if tokenizer.pad_token is None:
    tokenizer.pad_token = tokenizer.eos_token
model = AutoModelForCausalLM.from_pretrained(model_name)
model.config.pad_token_id = tokenizer.pad_token_id
model.to(device)  # type: ignore
print("Model loaded on", device)

# === helpers ===
def log_probs_from_logits(logits, labels):
    logp = F.log_softmax(logits, dim=-1)
    logp_label = torch.gather(logp, 2, labels.unsqueeze(2)).squeeze(-1)
    return logp_label

def sentence_prob(sentence_txt):
    inputs = tokenizer(sentence_txt, return_tensors='pt', return_attention_mask=True)
    input_ids = inputs['input_ids'].to(device)
    attention_mask = inputs['attention_mask'].to(device)
    with torch.no_grad():
        output = model(input_ids=input_ids, attention_mask=attention_mask)
        log_probs = log_probs_from_logits(output.logits[:, :-1, :], input_ids[:, 1:])
        seq_log_probs = torch.sum(log_probs)
    return seq_log_probs.cpu().numpy()  

def ask_model(prompt, max_new_tokens=50, temperature=0.7):
    inputs = tokenizer(prompt, return_tensors='pt', return_attention_mask=True)
    input_ids = inputs['input_ids'].to(device)
    attention_mask = inputs['attention_mask'].to(device)
    with torch.no_grad():
        output_ids = model.generate(
            input_ids,
            attention_mask=attention_mask,
            max_new_tokens=max_new_tokens,
            temperature=temperature,
            do_sample=True,
            pad_token_id=tokenizer.pad_token_id
        )
    output_text = tokenizer.decode(output_ids[0], skip_special_tokens=True)
    return output_text

# === main loop ===
def random_equation(num_terms=2, prob_brackets=0.3):
    ops = ['+', '-', '*', '/']
    equation_terms = []
    if num_terms < 1:
        num_terms = 1
    for _ in range(num_terms):
        if random.random() < prob_brackets:
            inner = random_equation(num_terms=max(1, num_terms//2), prob_brackets=prob_brackets/2)
            term = f"({inner.to_string()})"
        else:
            term = str(random.randint(1, 10))
        equation_terms.append(term)
    equation_str = equation_terms[0]
    for term in equation_terms[1:]:
        op = random.choice(ops)
        equation_str += f" {op} {term}"
    return equation.Equation(equation_str)




few_shot_examples = []

prompts = [
    ("Oblicz wartość wyrażenia: ", "\nWynik: "),
    ("Wartość wyrażenia matematycznego ", "to "),
    {"Podaj wynik działania: ", "\nWynik to: "},
    ("Calculate the result of the expression: ", "\nResult: "),
    {"", " = "},
]


def test_prompt(prompt_idx, num_tests=100, eq_params=(4, 0.3), eq=None):
    prompt_start, prompt_end = prompts[prompt_idx]
    prompt_few_shot = ""

    for eq_str, ans in few_shot_examples:
        prompt_few_shot += f"{prompt_start}{eq_str}{prompt_end}{ans}\n"

    correct = 0
    answeres = []
    for _ in tqdm(range(num_tests), desc=f"Testing prompt {prompt_idx}", leave=False):
        if eq == None: eq = random_equation(num_terms=eq_params[0], prob_brackets=eq_params[1])
        full_prompt = prompt_few_shot + prompt_start + eq.to_string() + prompt_end
        response = ask_model(full_prompt, max_new_tokens=50)
        match = re.search(r'(-?\d+)', response)
        if match:
            model_answer = int(match.group(1))
            answeres.append((model_answer, eq.evaluate()))
            if model_answer == eq.evaluate():
                correct += 1

        # tqdm.write(f"Prompt accuracy: {correct}/{i + 1} = {correct / (i + 1):.2%}")
    accuracy = correct / num_tests

    return accuracy, answeres


def dist(a, b):
    return min(abs(a - b) / 100, 1) if b != 0 else abs(a - b)


if __name__ == "__main__":
    # few_shot_examples = [
    #     ("3 + 5 * 2", 13),
    #     ("(4 + 6) / 2", 5),
    # ]

    # eq = equation.Equation("2 + 2 * 2")

    # for method in range(len(prompts)):
    #     acc, ans = test_prompt(method, num_tests=1, eq=eq)
    #     print(f"Prompt {method} accuracy: {acc:.2%}, distance: {dist(ans[0][0], ans[0][1]):.4f}")
    #     print(f"Model answers: {ans}")


    FEWHOT_EXAMPLES = 3
    PROMPT_TEST_RANGE = (len(prompts) - 1, len(prompts) - 1)
    RANDOM_EQ_PARAMS = (2, 0.0)

    for i in range(FEWHOT_EXAMPLES):
        eq = random_equation(num_terms=RANDOM_EQ_PARAMS[0], prob_brackets=RANDOM_EQ_PARAMS[1])
        few_shot_examples.append((eq.to_string(), round(eq.evaluate(), 2)))
    print("Few-shot examples:")
    for eq_str, ans in few_shot_examples:
        print(f"\t{eq_str} = {ans}")
    print("============\n")

    acc = []
    for i in tqdm(range(PROMPT_TEST_RANGE[0], PROMPT_TEST_RANGE[1] + 1), desc="Testing prompts"):
        # tqdm.write(f"Testing prompt {i}: {prompts[i]}")
        acc.append(test_prompt(i, num_tests=10, eq_params=RANDOM_EQ_PARAMS))
        # tqdm.write(f"Prompt {i} accuracy: {acc[-1][0]:.2%}")

    print("Prompt accuracies:")
    for i, (accuracy, answers) in enumerate(acc):
        ans_dist_total = 0
        for model_answer, correct_answer in answers:
            ans_dist = dist(model_answer, correct_answer)
            ans_dist_total += ans_dist

        print(f"Prompt {i}: {accuracy:.2%}, Avg answer distance: {ans_dist_total / len(answers):.4f}")

