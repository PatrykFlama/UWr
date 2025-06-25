[(back)](../)

# Lista 13 - hashowanie

## Zad 1 - Wyszukiwanie wzorca

Policz, ile razy słowo $P$ występuje w słowie $T$ jako podciąg (ciąg kolejnych znaków).

**Koncept rozwiązania:** Rabin-Karp (rolling hash)  
Zamiast porównywać wzorzec z każdym możliwym podciągiem tekstu, używamy haszowania do szybkiego porównania fragmentów tekstu z hashem wzorca.

### Szczegóły algorytmu

1. Obliczamy hash wzorca $P$.
2. Dla każdego przesunięcia długości $|P|$ w tekście $T$:
    - Obliczamy hash tego podciągu $T$.
    - Jeśli hashe są równe, porównujemy znaki dla potwierdzenia (collision check).
    - Zliczamy liczbę zgodnych dopasowań.

**Dlaczego działa:**  
Rolling hash pozwala aktualizować hash fragmentu tekstu w czasie stałym przy każdym przesunięciu - nie musimy przeliczać od zera.

**Złożoność czasowa:**  
- Hashowanie wzorca i pierwszego okna: $O(|P|)$  
- Iteracja po tekście: $O(|T|)$  
- Razem: $O(|T| + |P|)$

### Pseudokod

```python
def rabin_karp(T, P):
    n = len(T)
    m = len(P)
    B = 911
    MOD = 10**9 + 7

    base_powers = precompute_powers(m)
    hash_P = compute_hash(P, B, MOD)
    current_hash = compute_hash(T[0:m], B, MOD)

    matches = 0
    for i in range(n - m + 1):
        if current_hash == hash_P:
            if T[i:i+m] == P:
                matches += 1
        if i + m < n:
            current_hash = roll_hash(current_hash, T[i], T[i+m], B, base_powers[m-1], MOD)

    return matches
```


## Zad 2 - Minimalne przesunięcie cykliczne

Dla danego słowa $T$ znajdź jego najmniejszą leksykograficznie rotację - czyli taką, która po dowolnej liczbie cyklicznych przesunięć będzie najmniejsza słownikowo.

**Koncept rozwiązania:**  
Sklejamy słowo z samym sobą: $S = T + T$ - wtedy wszystkie rotacje $T$ są spójnymi podciągami $S$ długości $n$.

Hashujemy prefiksy: dzięki temu możemy w stałym czasie sprawdzać hash dowolnego fragmentu.

Porównujemy rotacje: porównujemy fragmenty $S[i:i+n]$ i $S[j:j+n]$ przez:
- binarne wyszukiwanie długości wspólnego prefiksu (LCP) na haszach,
- jeśli $LCP < n$, porównujemy znak rozstrzygający.

Iterujemy po $i = 1 \ldots n$, trzymając indeks najlepszej rotacji $best$.

**Dlaczego to działa:**  
Hash umożliwia szybkie porównanie fragmentów ($O(1)$), a wyszukiwanie binarne pozwala znaleźć pierwszy różny znak ($O(\log n)$). Dzięki temu każde porównanie rotacji działa w $O(\log n)$ zamiast $O(n)$.

**Złożoność czasowa:**  
- Obliczanie haszy prefiksowych: $O(n)$  
- Porównanie każdej rotacji: $O(\log n)$  
- Całość: $O(n \log n)$

### Pseudokod

```python
def find_min_rotation(T):
    S = T + T
    n = len(T)
    H = compute_prefix_hashes(S)
    powers = compute_powers(len(S))

    best = 0
    for i in range(1, n + 1):
        lcp = binary_lcp(H, powers, best, i, n)
        if lcp < n and S[best + lcp] > S[i + lcp]:
            best = i

    return S[best : best + n]
```

## Zad 3 - Najdłuższy palindrom

Dla danego słowa $T$, znajdź najdłuższy spójny palindromiczny podciąg. Jeśli jest kilka, wypisz ten, który występuje najwcześniej.

**Koncept rozwiązania:**  
Dla każdego znaku $i$ w $T$ zakładamy środek palindromu (dla długości nieparzystej i parzystej).  
Wykonujemy binarne wyszukiwanie maksymalnego promienia $r$, dla którego:

- hash podłańcucha $[i - r,\, i + r]$ (lub $[i - r + 1,\, i + r]$ dla parzystych) jest palindromiczny,
- czyli hash tego fragmentu zgadza się z hashem analogicznego fragmentu w odwróconym $T$.

Używamy wcześniej obliczonych prefiksowych haszy oraz haszy słowa odwróconego $R = T[::-1]$, aby móc porównywać podłańcuchy.

**Dlaczego to działa:**  
Porównując hashe fragmentu $T[l:r]$ i odwróconego $R[n - r : n - l]$, możemy sprawdzić, czy fragment $T[l:r]$ jest palindromem — wystarczy, że jego hash zgadza się z hashem „odbicia” w $R$.

**Złożoność czasowa:**  
- Obliczanie haszy: $O(n)$  
- Dla każdego znaku próbujemy maksymalny promień: $O(\log n)$  
- Razem: $O(n \log n)$

### Pseudokod

```python
def longest_palindrome(T):
    n = len(T)
    R = T[::-1]
    H = prefix_hashes(T)
    RH = prefix_hashes(R)
    best = (0, 0)  # (start, length)

    for center in range(n):
        for parity in ['odd', 'even']:
            r = binary_search_max_radius(center, parity, H, RH)
            l = center - r + (1 if parity == 'even' else 0)
            length = 2 * r - (1 if parity == 'even' else 0)
            if length > best[1]:
                best = (l, length)

    return T[best[0] : best[0] + best[1]]
```



## Zad 4 - Porównanie podzbiorów

### Opis zadania

Mamy dwa ciągi liczb długości $N$, zawierające unikalne liczby. Otrzymujemy $Q$ zapytań, z których każde sprawdza, czy pewne dwa podciągi (z pierwszego i drugiego ciągu) są permutacjami tych samych elementów.

### Koncept rozwiązania

Ponieważ liczby są unikalne w każdym ciągu, porównywanie permutacji dwóch podciągów można zredukować do porównania multizbiorów wartości.

Najlepszym sposobem, by to zrobić efektywnie, jest użycie haszowania zbiorów:

- Wylosujemy unikalne liczby (duże, losowe) dla każdej wartości z zakresu $[0, 1\,000\,000]$.
- Każdy element mapujemy do tej wartości i zamiast sumować wartości elementów, tworzymy prefiksową sumę hashy (lub używamy operacji XOR).
- Dla danego przedziału $[l, r]$ wartość hasza to $\text{pref}[r] - \text{pref}[l-1]$.
- Porównując dwie takie wartości — z dużym prawdopodobieństwem stwierdzimy, czy są to te same zbiory.

### Dlaczego działa?

Elementy są unikalne — więc permutacja to po prostu zbiór liczb w innym porządku.

Hash wartości zapewnia unikalność (z dużym prawdopodobieństwem).

XOR jest dobry, ale suma losowych hashy z modulo (np. $10^{18}+7$) daje mniejsze ryzyko kolizji.

Prefiksy pozwalają odpowiadać na każde zapytanie w czasie stałym.

### Złożoność

- **Czas:** $O(N + Q)$
- **Pamięć:** $O(N + \max(a_i))$

### Pseudokod

```python
# hash_val[a] = losowa wartość z zakresu 64-bit (dla każdej wartości 0..1e6)
for oba ciągi:
    pref[i] = pref[i-1] + hash_val[a[i]]

for każde zapytanie (l1, r1, l2, r2):
    hash1 = pref1[r1] - pref1[l1-1]
    hash2 = pref2[r2] - pref2[l2-1]
    if hash1 == hash2:
        wypisz "TAK"
    else:
        wypisz "NIE"
```
