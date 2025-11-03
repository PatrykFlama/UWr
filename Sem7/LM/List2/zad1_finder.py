import argparse
import copy
import json
import random
import re
import sys
from tqdm.auto import tqdm

import zad1

# --- configuration ---
POP_SIZE = 24
GENERATIONS = 25
ELITE_FRACTION = 0.25
MUTATION_RATE = 0.35
TESTS_PER_EVAL = 12        # equations per candidate evaluation (lower = faster, noisier)
EQ_PARAMS = (2, 0.0)
TEMPS = [0.0, 0.3, 0.7, 1.0]
SAVE_PATH = "prompt_finder_evo_results.json"

# small built-in wordlist - dict could be used
BUILTIN_WORDS = [
    "Oblicz", "Ile", "wynosi", "wartość", "wyrażenia", "Podaj", "wynik", "działania",
    "Calculate", "Evaluate", "result", "expression", "Answer", "Odpowiedź", "proszę",
    "suma", "razem", "ilewynosi", "proszępodaj", "policz"
]

RE_NUMBER = re.compile(r'(-?\d+(?:\.\d+)?)')


def load_wordlist(path=None):
    if not path:
        return BUILTIN_WORDS
    try:
        with open(path, "r", encoding="utf-8") as f:
            words = [line.strip() for line in f if line.strip()]
            return words if words else BUILTIN_WORDS
    except Exception:
        return BUILTIN_WORDS


def parse_number(s):
    m = RE_NUMBER.search(s)
    if not m:
        return None
    try:
        return float(m.group(1))
    except Exception:
        return None


def score_candidate(start, end, temperature, num_tests=TESTS_PER_EVAL, eq_params=EQ_PARAMS):
    total = 0.0
    for _ in range(num_tests):
        eq = zad1.random_equation(num_terms=eq_params[0], prob_brackets=eq_params[1])
        prompt = f"{start}{eq.to_string()}{end}"
        try:
            resp = zad1.ask_model(prompt, max_new_tokens=40, temperature=temperature)
        except Exception:
            # model call failed - punish heavily
            total += 1.0
            continue
        num = parse_number(resp)
        if num is None:
            total += 1.0
        else:
            total += zad1.dist(num, eq.evaluate())
    return total / num_tests


# genome: dict { "start": str, "end": str, "temp": float }
def random_genome(starts, ends, temps, words):
    start = random.choice(starts) if random.random() < 0.6 else " ".join(random.choices(words, k=random.randint(0, 4))).strip()
    end = random.choice(ends) if random.random() < 0.6 else " ".join(random.choices(words, k=random.randint(0, 3))).strip()
    return {"start": (start + " ").strip(), "end": (" " + end).strip() if end else end, "temp": random.choice(temps)}


def mutate_genome(g, words):
    out = copy.deepcopy(g)
    # mutate temperature
    if random.random() < 0.2:
        out["temp"] = random.choice(TEMPS)
    # pick start or end to mutate
    for key in ("start", "end"):
        if random.random() < MUTATION_RATE:
            tokens = out[key].split()
            op = random.choice(["insert", "delete", "replace", "shuffle", "append_punct"])
            if op == "insert":
                pos = random.randint(0, len(tokens))
                tokens.insert(pos, random.choice(words))
            elif op == "delete" and tokens:
                tokens.pop(random.randrange(len(tokens)))
            elif op == "replace" and tokens:
                tokens[random.randrange(len(tokens))] = random.choice(words)
            elif op == "shuffle" and len(tokens) > 1:
                i, j = random.sample(range(len(tokens)), 2)
                tokens[i], tokens[j] = tokens[j], tokens[i]
            elif op == "append_punct":
                if tokens:
                    tokens[-1] = tokens[-1].rstrip(" :.\n") + random.choice(["?", ":", " ="])
                else:
                    tokens.append(random.choice(words))
            out[key] = (" ".join(tokens)).strip()
            # ensure spaces around start/end
            if key == "start" and out[key]:
                out[key] = out[key].strip() + " "
            if key == "end" and out[key]:
                out[key] = " " + out[key].strip()
    return out


def crossover(a, b):
    child = {"start": "", "end": "", "temp": a["temp"] if random.random() < 0.5 else b["temp"]}
    for key in ("start", "end"):
        ta = a[key].split()
        tb = b[key].split()
        if not ta and not tb:
            child[key] = ""
            continue
        cut_a = len(ta) // 2
        cut_b = len(tb) // 2
        new_tokens = ta[:cut_a] + tb[cut_b:]
        child[key] = (" " .join(new_tokens)).strip()
        if key == "start" and child[key]:
            child[key] = child[key] + " "
        if key == "end" and child[key]:
            child[key] = " " + child[key]
    return child


def initialize_population(starts, ends, temps, words):
    pop = []
    # seed with zad1.prompts and some grid combos
    for p in zad1.prompts:
        if isinstance(p, (list, tuple)) and len(p) == 2:
            for t in temps:
                pop.append({"start": p[0], "end": p[1], "temp": t})
    # fill to POP_SIZE
    while len(pop) < POP_SIZE:
        pop.append(random_genome(starts, ends, temps, words))
    return pop[:POP_SIZE]


def evolve(starts, ends, temps, words, generations=GENERATIONS):
    pop = initialize_population(starts, ends, temps, words)
    history = []
    for gen in range(1, generations + 1):
        scores = []
        it = tqdm(range(len(pop)), desc=f"Gen {gen}/{generations}", leave=False)
        for i in it:
            g = pop[i]
            score = score_candidate(g["start"], g["end"], g["temp"])
            scores.append((score, g))
        scores.sort(key=lambda x: x[0])  # lower is better
        best_score, best_genome = scores[0]
        history.append({"gen": gen, "best_score": best_score, "best": best_genome})
        # selection: keep elites
        elite_count = max(1, int(len(pop) * ELITE_FRACTION))
        elites = [g for _, g in scores[:elite_count]]
        # generate offspring
        new_pop = elites.copy()
        while len(new_pop) < POP_SIZE:
            if random.random() < 0.3:
                # crossover
                a = random.choice(elites)
                b = random.choice(pop)
                child = crossover(a, b)
            else:
                parent = random.choice(elites)
                child = mutate_genome(parent, words)
            # extra random mutation chance
            if random.random() < 0.1:
                child = mutate_genome(child, words)
            new_pop.append(child)
        pop = new_pop
        # persist intermediate best
        with open(SAVE_PATH, "w", encoding="utf-8") as f:
            json.dump({"history": history, "population_sample": pop, "best_so_far": history[-1]}, f, ensure_ascii=False, indent=2)
    return history


def main(argv):
    global POP_SIZE, GENERATIONS, TESTS_PER_EVAL

    parser = argparse.ArgumentParser(description="Evolve prompts for zad1 model.")
    parser.add_argument("--words", type=str, help="path to wordlist file (one word per line)")
    parser.add_argument("--pop", type=int, default=POP_SIZE)
    parser.add_argument("--gens", type=int, default=GENERATIONS)
    parser.add_argument("--tests", type=int, default=TESTS_PER_EVAL)
    args = parser.parse_args(argv)

    POP_SIZE = args.pop
    GENERATIONS = args.gens
    TESTS_PER_EVAL = args.tests

    words = load_wordlist(args.words)
    starts = [
        "Oblicz wartość wyrażenia: ",
        "Wartość wyrażenia matematycznego ",
        "Podaj wynik działania: ",
        "Calculate the result of the expression: ",
        "",
        "Ile wynosi: ",
        "Oblicz: ",
        "Evaluate: ",
    ]
    ends = [
        "\nWynik: ",
        " to ",
        "\nWynik to: ",
        "\nResult: ",
        " = ",
        "\nAnswer: ",
        "\nOdpowiedź: ",
        ": ",
    ]

    random.seed(0)
    print("Starting evolutionary search (model must be loaded in zad1)...")
    history = evolve(starts, ends, TEMPS, words, generations=GENERATIONS)

    best = history[-1]["best"]
    print("Best genome found:")
    print(f"  temp: {best['temp']}")
    print(f"  start: {repr(best['start'])}")
    print(f"  end:   {repr(best['end'])}")
    with open(SAVE_PATH, "w", encoding="utf-8") as f:
        json.dump({"history": history, "best": best}, f, ensure_ascii=False, indent=2)
    print(f"Results saved to {SAVE_PATH}")


if __name__ == "__main__":
    main(sys.argv[1:])
