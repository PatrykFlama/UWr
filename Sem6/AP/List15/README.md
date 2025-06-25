[(back)](../)

# Lista 15 - teoria gier




## Zad 1 - Kamienie

**Opis:**  
Na stosach kamieni gracze na przemian zdejmują dowolną liczbę kamieni z jednego stosu. Przegrywa ten, kto nie może wykonać ruchu.

**Koncept:**  
Klasyczna gra Nim. Wygrywa gracz zaczynający, jeśli $XOR$ ilości kamieni na wszystkich stosach jest różny od $0$.

$XOR$ jest sumą bitową bez przeniesienia.

Jeśli $XOR = 0$, to gracz zaczynający przegrywa przy optymalnej grze przeciwnika.

**Dlaczego działa:**  
Właściwości $XOR$ w Nim pozwalają na prostą ocenę pozycji jako wygrywającej lub przegrywającej.

**Złożoność:**  
$O(N)$ na test (sumarycznie $O(\sum N)$).

**Pseudokod:**
```python
for each test:
    xor_sum = 0
    for ai in stacks:
        xor_sum ^= ai
    if xor_sum != 0:
        print("first")
    else:
        print("second")
```

---

## Zad 2 - Kamienie 2

**Opis:**  
Gracze mogą zdjąć $1$, $3$ lub $4$ kamienie ze stosu. Kto nie może ruchu — przegrywa.

**Koncept:**  
Problem typu "Nim z ograniczonymi ruchami" — wyliczamy wartości Grundy (nimbers) dla każdej liczby kamieni.

Grundy dla stanu $x = \mathrm{mex}$ (minimum excluded value) z Grundy stanów po dozwolonych ruchach ($x-1$, $x-3$, $x-4$).

$XOR$ujemy wyniki dla wszystkich stosów.

$XOR \neq 0$ → wygrywa pierwszy.

**Złożoność:**  
Preobliczenie Grundy do $max\_a_i$ (do $10^6$) w $O(max\_a_i)$, potem $O(N)$ na test.

**Pseudokod:**
```python
# Precompute grundy[0..max_ai]:
grundy[0] = 0
for i in 1..max_ai:
    moves = []
    if i-1 >= 0: moves.append(grundy[i-1])
    if i-3 >= 0: moves.append(grundy[i-3])
    if i-4 >= 0: moves.append(grundy[i-4])
    grundy[i] = mex(moves)

for each test:
    xor_sum = 0
    for ai in stacks:
        xor_sum ^= grundy[ai]
    if xor_sum != 0:
        print("first")
    else:
        print("second")
```

---

## Zad 3 - Kamienie na szachownicy

**Opis:**  
Kamienie na planszy $1000 \times 1000$. Ruchy przypominają skok konia szachowego w określonych kierunkach. Przegrywa ten, kto nie może wykonać ruchu.

**Koncept:**  
Każdy kamień to osobna niezależna gra. Obliczamy Grundy dla pozycji $(w, k)$ rekurencyjnie z memoizacją.  
Wynik to $XOR$ wszystkich wartości Grundy dla kamieni. $XOR \neq 0$ → wygrywa pierwszy.

**Wyjaśnienie:**  
Gra sum jest sumą gier niezależnych — łączenie przez $XOR$ wartości Grundy.

**Złożoność:**  
Memoizacja dla maksymalnie $1000 \times 1000$ pozycji: $O(10^6)$.

**Pseudokod:**
```python
def grundy(w, k):
    if (w, k) out of bounds: return 0
    if memo[w][k] set: return memo[w][k]
    moves = [(w-2, k-1), (w-2, k+1), (w-1, k-2), (w+1, k-2)]
    next_states = [grundy(x, y) for (x, y) in moves if valid]
    memo[w][k] = mex(next_states)
    return memo[w][k]

for each test:
    xor_sum = 0
    for each stone (w, k):
        xor_sum ^= grundy(w, k)
    if xor_sum != 0:
        print("first")
    else:
        print("second")
```

---

## Zad 4 - Kamienie na planszy

**Opis:**  
Plansza $1 \times N$. Gracze na zmianę kładą kamienie na pustych polach, ale nie można położyć kamienia obok innego kamienia.

**Koncept:**  
To gra na łańcuchu z ograniczeniem — problem podobny do "Nim z przerwami".  
Można rozłożyć planszę na segmenty, wyliczyć Grundy dla każdego segmentu.  
Używamy DP do wyliczenia wartości dla długości segmentów ($dp[i]$ = wartość gry na łańcuchu długości $i$).

**Złożoność:**  
$O(N)$ na test.

**Pseudokod:**
```python
# Precompute dp[0..N]:
dp[0] = 0
dp[1] = 1
for i in 2..N:
    # oblicz mex z możliwych ruchów (umieszczanie kamieni)
    dp[i] = ...

# dla pustej planszy wynik to dp[N]
if dp[N] != 0:
    print("first")
else:
    print("second")
```

---

## Zad 5 - Inne kamienie

**Opis:**  
Można zdjąć po jednym kamieniu z dowolnego wybranego zbioru stosów (równocześnie).

**Koncept:**  
Każdy ruch to usunięcie kamienia z dowolnej liczby stosów. Gra jest równoważna Nim, ale z ruchem usuwającym z dowolnej liczby stosów jednocześnie (czyli inaczej niż klasyczny Nim).  
Okazuje się, że decyduje parzystość sumy kamieni.

**Złożoność:**  
$O(N)$ na test.

**Pseudokod:**
```python
for each test:
    total = sum of ai
    if total % 2 == 1:
        print("first")
    else:
        print("second")
```

---

## Zad 6 - Kamienie na schodach

**Opis:**  
Gracze mogą przenosić kamienie z wyższego stopnia na niższy (stopnie ponumerowane od $1$ do $N$).

**Koncept:**  
Analiza gry pokazuje, że można sprowadzić pozycję do $XOR$ różnic między stosami:
$$
xor = a_1 \oplus (a_2 - a_1) \oplus (a_3 - a_2) \oplus \dots \oplus (a_N - a_{N-1})
$$
Jeśli wynik $XOR \neq 0$, wygrywa pierwszy.

**Złożoność:**  
$O(N)$ na test.

**Pseudokod:**
```python
for each test:
    xor_sum = a[1]
    for i in 2..N:
        xor_sum ^= (a[i] - a[i-1])
    if xor_sum != 0:
        print("first")
    else:
        print("second")
```

---

## Podsumowanie

- **Zad 1:** Klasyczny Nim ($XOR$ stosów).
- **Zad 2:** Nim z ograniczonymi ruchami, używamy Grundy.
- **Zad 3:** Gra z ruchem konia, Grundy per pozycja.
- **Zad 4:** Gra na łańcuchu z ograniczeniem sąsiedztwa, DP.
- **Zad 5:** Usuwanie kamieni z dowolnej liczby stosów na raz → parzystość sumy.
- **Zad 6:** $XOR$ różnic między stosami.

