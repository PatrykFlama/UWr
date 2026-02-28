from collections import defaultdict
from math import gcd
from pathlib import Path


plik = Path(__file__).with_name("SzyfrNieznany.txt")
tekst = plik.read_text(encoding="utf-8")
tekst = "".join(ch for ch in tekst.upper() if "A" <= ch <= "Z")

n = 3
pozycje = defaultdict(list)
for i in range(len(tekst) - n + 1):
    gram = tekst[i:i + n]
    pozycje[gram].append(i)

kandydaci_punkty = defaultdict(int)
powtorzenia = 0

for gram, miejsca in pozycje.items():
    if len(miejsca) >= 2:
        powtorzenia += 1
        roznice = []
        for i in range(len(miejsca) - 1):
            d = miejsca[i + 1] - miejsca[i]
            roznice.append(d)
        nwd = roznice[0]
        for d in roznice[1:]:
            nwd = gcd(nwd, d)

        if nwd > 1:
            for s in range(2, nwd + 1):
                if nwd % s == 0:
                    kandydaci_punkty[s] += 1

print("Długość szyfrogramu:", len(tekst))
print("Liczba powtarzających się trójgramów:", powtorzenia)

ranking = sorted(kandydaci_punkty.items(), key=lambda x: (-x[1], x[0]))

print("\nKandydaci długości klucza s:")
for s, punkty in ranking[:10]:
    print("s =", s, "punkty =", punkty)

print("\n--- zgadywanie klucza dla wybranego s ---")
freq_ang = [
    0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
    0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
    0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
    0.00978, 0.02360, 0.00150, 0.01974, 0.00074,
]

ile_sprawdzac = min(5, len(ranking))

for nr in range(ile_sprawdzac):
    s = ranking[nr][0]

    kolumny = []
    for i in range(s):
        kolumny.append(tekst[i::s])

    klucz = ""
    suma_wynikow = 0.0

    for i in range(s):
        kol = kolumny[i]
        najlepszy_k = 0
        najlepszy_wynik = -1.0

        for k in range(26):
            licz = [0] * 26
            for ch in kol:
                x = ord(ch) - ord("A")
                m = (x - k) % 26
                licz[m] += 1

            pj = [v / len(kol) for v in licz]
            wynik = 0.0
            for j in range(26):
                wynik += pj[j] * freq_ang[j]

            if wynik > najlepszy_wynik:
                najlepszy_wynik = wynik
                najlepszy_k = k

        litera_klucza = chr(ord("A") + najlepszy_k)
        klucz += litera_klucza
        suma_wynikow += najlepszy_wynik

    sredni_wynik = suma_wynikow / s

    odszyfrowany = []
    for i, ch in enumerate(tekst):
        k = ord(klucz[i % s]) - ord("A")
        x = ord(ch) - ord("A")
        m = (x - k) % 26
        odszyfrowany.append(chr(m + ord("A")))

    odszyfrowany = "".join(odszyfrowany)

    print("\nKandydat", nr + 1, "| s =", s, "| średnie dopasowanie =", round(sredni_wynik, 5))
    print("Klucz:", klucz)
    print("Tekst:", odszyfrowany)
