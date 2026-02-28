import math
import random
from collections import Counter
from pathlib import Path

def load_words(path: Path, min_len=2, max_len=20):
    word_set = set()
    for line in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        if line and line[0].isupper():
            # Skip proper nouns and acronyms from this dictionary variant.
            continue
        w = "".join(ch for ch in line.upper() if "A" <= ch <= "Z")
        if min_len <= len(w) <= max_len:
            word_set.add(w)
    word_set.update({"A", "I"})
    return word_set

base = Path(__file__).parent
words_path = base / "words"
words = load_words(words_path)

letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
letter_freq = Counter()
for w in words:
    letter_freq.update(w)

total_letters = sum(letter_freq.values())
letter_freq = {ch: count / total_letters for ch, count in letter_freq.items() if "A" <= ch <= "Z"}

def entropy(freqs):
    return -sum(p * math.log2(p) for p in freqs.values())

print("Letter frequencies:")
for ch in letters:
    print(f"{ch}: {letter_freq.get(ch, 0):.4f}")

print(f"Entropy of letter distribution: {entropy(letter_freq):.4f}")

random_freq = (1 / len(letters))
print(f"Entropy of uniform distribution: {entropy({ch: random_freq for ch in letters}):.4f}")

