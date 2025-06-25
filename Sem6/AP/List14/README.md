[(back)](../)

# Lista 14 - algorytmy tekstowe
## Zad 1 – Wyszukiwanie wzorca

**Cel:** Policz, ile razy wzorzec $P$ występuje jako ciągły fragment w tekście $T$.

**Koncept rozwiązania:** Rabin-Karp (haszowanie tekstów)

Wykorzystujemy rolling hash (Rabin-Karp), aby:

- obliczyć hash wzorca $P$,
- policzyć hashe wszystkich spójnych fragmentów długości $|P|$ w $T$,
- porównać je w czasie stałym.

Ponieważ chcemy znaleźć wszystkie wystąpienia $P$ w $T$, przesuwamy "okno" długości $|P|$ po $T$ i sprawdzamy, czy hash się zgadza.

**Dlaczego to działa?**

Haszowanie zamienia stringi na liczby, które można szybko porównywać.

Rabin-Karp umożliwia obliczanie kolejnych hashy w czasie stałym, wykorzystując rekurencyjną formułę i preobliczone potęgi podstawy modulo.

**Złożoność czasowa:**

- Preprocessing hashów: $O(n)$
- Iteracja i porównania: $O(n)$

Łącznie: $O(n + m)$, gdzie $n = |T|$, $m = |P|$.

## Zad 2 -  Prefikso-sufiksy

**Opis:**  
Znajdź wszystkie długości niepustych fragmentów słowa $T$, które są zarówno prefiksami, jak i sufiksami i nie są całym słowem.

**Koncept:**  
Użyj tablicy $\pi$ (prefix function) z algorytmu KMP.  
Dla pozycji $i$ tablica $\pi[i]$ przechowuje długość najdłuższego prawidłowego prefikso-sufiksu dla $T[0..i]$.

Z ostatniej wartości $\pi[n-1]$ iteracyjnie odczytujemy wszystkie prefikso-sufiksy (bo: jeśli coś jest prefikso-sufiksem całego słowa, to jego prefiks też jest takim dla krótszego słowa).

**Złożoność:**  
- Czas: $O(n)$  
- Pamięć: $O(n)$

---

## Zad 3 -  Sklejanie słów

**Opis:**  
Policz, na ile sposobów można rozciąć tekst $T$, tak by każde cięcie odpowiadało słowu z danego słownika.

**Koncept:**  
Użyj dynamicznego programowania i trie'a do szybkiego dopasowywania słów słownika.

Niech $dp[i]$ oznacza liczbę sposobów rozcięcia $T[0..i-1]$.

Przechodząc po Trie od końca słowa $T[0..i-1]$, sprawdzamy, czy istnieje słowo kończące się na tej pozycji.

**Złożoność:**  
- Czas: $O(N \cdot \text{avg\_len})$ (średnia długość słowa)  
- Pamięć: $O(\sum \text{długości słów})$

**Pseudokod:**
```py
build trie from dictionary
dp[0] = 1
for i = 1 to N:
    node = trie root
    for j = i downto max(0, i - max_word_len):
        if T[j] not in node:
            break
        node = node[T[j]]
        if node is_terminal:
            dp[i] += dp[j]
output dp[N]
```

---
## Zad 4 -  Różnice symetryczne

**Opis:**  
Policz liczbę par $(i, j)$ ($i < j$), dla których $a[i] \oplus a[j] \geq K$.

**Koncept:**  
Użyj Trie’a binarnego i przetwarzaj tablicę od lewej.

Dla każdej liczby $a[i]$:

Zlicz, ile wcześniej dodanych liczb spełnia $(a[i] \oplus x) \geq K$, używając trie i sprawdzając bit po bicie.

**Wyjaśnienie:**  
W Trie trzymamy liczby jako bity. Wchodząc w Trie dla $a[i]$, sprawdzamy, ile liczb $x$ pasuje tak, by $(a[i] \oplus x) \geq K$. Działa to w $O(\log(\text{MAX}_A))$.

**Złożoność:**  
- Czas: $O(N \cdot \log A)$, $A \leq 2^{30}$  
- Pamięć: $O(N \cdot \log A)$

**Pseudokod:**
```py
function insert(trie, number)
function count_ge(trie, number, K)

res = 0
trie = empty
for x in A:
    res += count_ge(trie, x, K)
    insert(trie, x)
output res
```


## Zad 5 (bonus) – Zliczanie wzorców

**Opis:**  
Dany jest tekst $T$ oraz słownik $K$ słów.  
Trzeba policzyć, ile razy każde słowo ze słownika występuje w tekście $T$.

**Koncept:**  
Zastosuj automat Aho-Corasick:

- Budujesz trie ze wszystkich słów słownika.
- Następnie liczysz linki sufiksowe (fail links) między wierzchołkami, co pozwala szybko przechodzić w tekście między stanami trie.
- Przechodzisz tekst $T$ znak po znaku, poruszając się po automacie, i za każdym razem, kiedy dochodzisz do końca wzorca, inkrementujesz licznik wystąpień.
- Po przetworzeniu tekstu propagujesz wyniki przez linki sufiksowe, by uwzględnić wzorce, które są sufiksami innych wzorców.

**Dlaczego to działa?**  
Aho-Corasick to uogólnienie automatu KMP dla wielu wzorców na raz, pozwalające przeszukać tekst jedną liniową iteracją, jednocześnie sprawdzając obecność wszystkich wzorców. Linki sufiksowe pozwalają szybko "cofać się" w automacie, gdy brak dopasowania.

**Złożoność:**  
- Budowa trie i linków sufiksowych: $O(M)$
- Przetwarzanie tekstu: $O(N)$
- Propagacja wyników: $O(M)$

Łącznie: $O(N + M)$

**Pseudokod:**
```py
build_trie(words)                    # budowa trie ze słów
compute_fail_links()                 # obliczenie linków sufiksowych

counts = array of zeros length K     # licznik wystąpień dla każdego słowa
state = 0                           # start w automacie

for c in T:
    state = next_state(state, c)    # przejście automatu
    temp = state
    while temp != 0:
        if terminal[temp]:
            counts[word_id[temp]] += 1  # inkrementuj wystąpienie
        temp = fail_link[temp]

output counts
```
