from math import gcd
from pathlib import Path


plik = Path(__file__).with_name("SzyfrAfiniczny.txt")
tekst = plik.read_text(encoding="utf-8")
tekst = "".join(ch for ch in tekst.upper() if "A" <= ch <= "Z")

freq_ang = [
    0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
    0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
    0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
    0.00978, 0.02360, 0.00150, 0.01974, 0.00074,
]

print("Długość szyfrogramu:", len(tekst))
print("\nBrute-force szyfru afinicznego: c = a*m + b (mod 26)")

kandydaci = []

for a in range(1, 26):
    if gcd(a, 26) != 1:
        continue

    a_inv = None
    for x in range(26):
        if (a * x) % 26 == 1:
            a_inv = x
            break

    for b in range(26):
        jawny = []
        for ch in tekst:
            c = ord(ch) - ord("A")
            m = (a_inv * (c - b)) % 26
            jawny.append(chr(m + ord("A")))

        jawny = "".join(jawny)
        licz = [0] * 26
        for ch in jawny:
            licz[ord(ch) - ord("A")] += 1

        p = [v / len(jawny) for v in licz]
        score = 0.0
        for j in range(26):
            score += p[j] * freq_ang[j]

        kandydaci.append((score, a, b, jawny))

kandydaci.sort(key=lambda x: x[0], reverse=True)

for score, a, b, jawny in kandydaci:
    print("a =", a, "b =", b)
    print(round(score, 6))
    print(jawny)
    print()

print("Liczba wszystkich kandydatów:", len(kandydaci))
