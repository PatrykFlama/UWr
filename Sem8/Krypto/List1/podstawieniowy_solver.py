import math
import random
from collections import Counter
from pathlib import Path


def load_cipher_and_hint(path: Path):
    raw = path.read_text(encoding="utf-8", errors="ignore")
    lines = raw.splitlines()

    cipher_lines = []

    for line in lines:
        s = line.strip()
        if not s:
            continue
        if "kodem" in s.lower():
            break
        cipher_lines.append(s.upper())

    cipher = "".join(ch for ch in "".join(cipher_lines) if "A" <= ch <= "Z")
    fixed = {"F": "W"}

    return cipher, fixed


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

def decode_with_map(text, c2p):
    return "".join(c2p.get(ch, "?") for ch in text)


def word_break_score(text, words, max_len=20):
    n = len(text)
    neg_inf = -10**9
    dp = [neg_inf] * (n + 1)
    prev = [-1] * (n + 1)
    typ = [""] * (n + 1)
    dp[0] = 0

    for i in range(n):
        if dp[i] <= neg_inf // 2:
            continue

        # Treat one char as unknown fragemnt
        if dp[i] - 1.2 > dp[i + 1]:
            dp[i + 1] = dp[i] - 1.2
            prev[i + 1] = i
            typ[i + 1] = "?"

        lim = min(max_len, n - i)
        for L in range(2, lim + 1):
            w = text[i:i + L]
            if w in words:
                bonus = (L * L)
                score = dp[i] + bonus
                if score > dp[i + L]:
                    dp[i + L] = score
                    prev[i + L] = i
                    typ[i + L] = "W"

    # Reconstruct a segmentation
    chunks = []
    pos = n
    while pos > 0 and prev[pos] != -1:
        i = prev[pos]
        fragment = text[i:pos]
        chunks.append(fragment)
        pos = i
    chunks.reverse()

    return dp[n], " ".join(chunks)


def initial_key(cipher, fixed):
    letters = list("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    # Sorted by freq in english
    eng_order = "ETAOINSHRDLCUMWFGYPBVKJXQZ"
    c2p = {}
    used_plain = set()

    for c, p in fixed.items():
        c2p[c] = p
        used_plain.add(p)

    # Init letters by freq in cipher
    by_freq = [ch for ch, _ in Counter(cipher).most_common()]
    for c in letters:
        if c not in by_freq:
            by_freq.append(c)

    pool = [ch for ch in eng_order if ch not in used_plain]
    for c in by_freq:
        if c in c2p:
            continue
        if pool:
            c2p[c] = pool.pop(0)

    # Fill letters that didnt appear
    missing_plain = [ch for ch in letters if ch not in set(c2p.values())]
    random.shuffle(missing_plain)
    for c in letters:
        if c not in c2p:
            c2p[c] = missing_plain.pop()
    return c2p


def solve(cipher, fixed, words, restarts=18, steps=2500):
    letters = list("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    movable = [c for c in letters if c not in fixed]
    best = []

    global_best = -10**18

    for _ in range(restarts):
        c2p = initial_key(cipher, fixed)
        plain = decode_with_map(cipher, c2p)
        cur_score, segmented = word_break_score(plain, words)

        temp = 8.0
        for _ in range(steps):
            a, b = random.sample(movable, 2)
            c2p[a], c2p[b] = c2p[b], c2p[a]

            p2 = decode_with_map(cipher, c2p)
            score2, s2 = word_break_score(p2, words)
            diff = score2 - cur_score

            accept = diff >= 0 or random.random() < math.exp(diff / max(temp, 0.001))
            if accept:
                plain = p2
                segmented = s2
                cur_score = score2
            else:
                c2p[a], c2p[b] = c2p[b], c2p[a]

            temp *= 0.9992

            if cur_score > global_best:
                global_best = cur_score
                best.append((cur_score, plain, segmented, dict(c2p)))

    best.sort(key=lambda x: x[0], reverse=True)
    return best[:5]


base = Path(__file__).parent
cipher_path = base / "SzyfrPodst.txt"
words_path = base / "words"

cipher, fixed = load_cipher_and_hint(cipher_path)
words = load_words(words_path)
best = solve(cipher, fixed, words)

print("Długość szyfrogramu:", len(cipher))
print("Rozmiar słownika:", len(words))
print()

for i, (score, plain, segmented, mapping) in enumerate(best, 1):
    print("=================")
    print("Tekst:")
    print(plain)
    print("\nSegmentacja:")
    print(segmented)
    print("\nMapa:")
    print(" ".join(f"{c}->{mapping[c]}" for c in "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    print()


"""
Wynik:
I MAY NOT BE ABLE TO GROW FLOWERS BUT MY GARDEN PRODUCES JUST AS MANY DEAD LEAVES OLD OVERSHOES PIECES OF ROPE AND BUSHELS OF DEAD GRASS AS ANYBODYS AND TODAY I BOUGHT A WHEELBARROW TO HELP IN CLEARING IT U PI HAVE ALWAYS LOVED AND RESPECTED THE WHEELBARROW IT IS THE ONE WHEELED VEHICLE OF WHICH I AM PERFECT MASTER
"""
