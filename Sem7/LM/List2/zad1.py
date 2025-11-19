import torch
from transformers import AutoTokenizer, AutoModelForCausalLM
from torch.nn import functional as F
import re
import random
from tqdm.auto import tqdm
import equation
from model_lib import ModelUtils


# === Model ===
# model_name = 'eryk-mazus/polka-1.1b-chat'
model_name = 'flax-community/papuGaPT2'
model_utils = ModelUtils(model_name)

# === main loop ===
def random_equation(num_terms=2, prob_brackets=0.3, ops=['+', '-', '*', '/'], randint=(1, 10)):
    equation_terms = []
    if num_terms < 1:
        num_terms = 1
    for _ in range(num_terms):
        if random.random() < prob_brackets:
            inner = random_equation(num_terms=max(1, num_terms//2), prob_brackets=prob_brackets/2, ops=ops, randint=randint)
            term = f"({inner.to_string()})"
        else:
            term = str(random.randint(randint[0], randint[1]))
        equation_terms.append(term)
    equation_str = equation_terms[0]
    for term in equation_terms[1:]:
        op = random.choice(ops)
        equation_str += f" {op} {term}"
    return equation.Equation(equation_str)




few_shot_examples = []

prompts = [
    # ("Oblicz wartość wyrażenia: ", "\nWynik:"),
    ("Wartość wyrażenia matematycznego ", " to"),
    ("\nPodaj wynik działania: ", "\nWynik to:"),
    # ("Calculate the result of the expression: ", "\nResult:"),
    # ("", " ="),
]


def test_prompt(prompt_idx, tests):
    prompt_start, prompt_end = prompts[prompt_idx]
    prompt_few_shot = ""

    for eq_str, ans in few_shot_examples:
        prompt_few_shot += f"{prompt_start}{eq_str}{prompt_end} {ans}\n"

    correct = 0
    answeres = []
    for cur_eq in tqdm(tests, desc=f"Testing prompt {prompt_idx}", leave=False):
        full_prompt = prompt_few_shot + prompt_start + cur_eq.to_string() + prompt_end
        response = model_utils.ask_model(full_prompt, max_new_tokens=50, temperature=0.3)

        # print("Prompt:", full_prompt, "\nResponse:", response, "\n")

        # accept integers and floats, and comma decimal separators
        # prefer the last numeric token in the generated continuation (more robust)
        matches = re.findall(r'(-?\d+(?:[.,]\d+)?)', response)
        if matches:
            token = matches[-1]
            # normalize comma to dot and parse as float
            try:
                model_answer = float(token.replace(',', '.'))
            except ValueError:
                # skip this example if parsing failed
                continue
            correct_answer = cur_eq.evaluate()
            answeres.append((model_answer, correct_answer))
            # compare with a small tolerance to allow decimal formatting differences
            if abs(model_answer - correct_answer) < 1e-6:
                correct += 1
        else:
            # keep a record of no numeric match for debugging
            answeres.append((None, cur_eq.evaluate(), response))

        # tqdm.write(f"Prompt accuracy: {correct}/{i + 1} = {correct / (i + 1):.2%}")
    accuracy = correct / len(tests)

    return accuracy, answeres


def dist(a, b):
    return abs(a - b)


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
    PROMPT_TEST_RANGE = (0, len(prompts) - 1)
    PROMPT_TEST_EQUATIONS = 10
    RANDOM_EQ_PARAMS = (2, 0.0)
    OPS = ['/']
    RANDINT = (1, 100)
    PRINT_ANSWERS = True

    for i in range(FEWHOT_EXAMPLES):
        eq = random_equation(num_terms=RANDOM_EQ_PARAMS[0], prob_brackets=RANDOM_EQ_PARAMS[1], ops=OPS, randint=RANDINT)
        few_shot_examples.append((eq.to_string(), round(eq.evaluate(), 2)))
    print("Few-shot examples:")
    for eq_str, ans in few_shot_examples:
        print(f"\t{eq_str} = {ans}")
    print("============\n")

    test_equations = []
    for _ in range(PROMPT_TEST_EQUATIONS):
        eq = random_equation(num_terms=RANDOM_EQ_PARAMS[0], prob_brackets=RANDOM_EQ_PARAMS[1], ops=OPS, randint=RANDINT)
        test_equations.append(eq)

    acc = []
    # for i in tqdm(range(PROMPT_TEST_RANGE[0], PROMPT_TEST_RANGE[1] + 1), desc="Testing prompts"):
    for i in range(PROMPT_TEST_RANGE[0], PROMPT_TEST_RANGE[1] + 1):
        # tqdm.write(f"Testing prompt {i}: {prompts[i]}")
        acc.append(test_prompt(i, test_equations))
        # tqdm.write(f"Prompt {i} accuracy: {acc[-1][0]:.2%}")

    print("Prompt accuracies:")
    for i, (accuracy, answers) in enumerate(acc):
        ans_dist_total = 0
        for model_answer, correct_answer in answers:
            ans_dist = dist(model_answer, correct_answer)
            ans_dist_total += ans_dist

        print(f"Prompt {i}: {accuracy:.2%}, Avg answer distance: {ans_dist_total / len(answers):.4f}")

        if PRINT_ANSWERS:
            for i in range(len(answers)):
                model_answer, correct_answer = answers[i]
                print(f"\t{test_equations[i].to_string()} = \"{model_answer}\" ({correct_answer}) -> dist: {dist(model_answer, correct_answer):.4f}")
            print()
