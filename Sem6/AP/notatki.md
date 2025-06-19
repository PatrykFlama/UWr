---
title: Notatki z wykładu z Algorytmiki Praktycznej 2025

---

- [Przydatne w rozwiązywaniu zadań](#przydatne-w-rozwiązywaniu-zadań)
  - [Linijki przyspieszające w C++](#linijki-przyspieszające-w-c)
  - [Szukanie błędów w kodzie](#szukanie-błędów-w-kodzie)
  - [Duże tablice](#duże-tablice)
- [Wykład 1: Teoria liczb](#wykład-1-teoria-liczb)
    - [Rozszerzony algorytm Euklidesa](#rozszerzony-algorytm-euklidesa)
      - [Wywołanie $nwd(a,0)$](#wywołanie-nwda0)
      - [Wywołanie $nwd(b,a%b)$](#wywołanie-nwdbab)
    - [Znajdowanie odwrotności modularnej](#znajdowanie-odwrotności-modularnej)
      - [Metoda 1: Rozszerzony algorytm Euklidesa](#metoda-1-rozszerzony-algorytm-euklidesa)
      - [Metoda 2: Małe twierdzenie Fermata](#metoda-2-małe-twierdzenie-fermata)
    - [Funkcja Eulera](#funkcja-eulera)
      - [Jeśli $p$ jest pierwsza, to $\\phi(p)=p-1$.](#jeśli-p-jest-pierwsza-to-phipp-1)
      - [Jeśli $p$ jest pierwsza, to $\\phi(p^k)=p^k-p^{k-1}=p^k(1-\\frac{1}{p})$.](#jeśli-p-jest-pierwsza-to-phipkpk-pk-1pk1-frac1p)
- [Wykład 2: Union Find](#wykład-2-union-find)
    - [Dowód własności funkcji Eulera](#dowód-własności-funkcji-eulera)
      - [Jeśli $a,b$ są względnie pierwsze, to $\\phi(ab)=\\phi(a)\\phi(b)$](#jeśli-ab-są-względnie-pierwsze-to-phiabphiaphib)
    - [Chińskie twierdzenie o resztach](#chińskie-twierdzenie-o-resztach)
      - [Rozwiązanie](#rozwiązanie)
    - [Problem struktury zbiorów rozłącznych](#problem-struktury-zbiorów-rozłącznych)
      - [Technika łączenia mniejszego zbioru do większego](#technika-łączenia-mniejszego-zbioru-do-większego)
      - [Union Find](#union-find)
      - [Technika kompresji ścieżek.](#technika-kompresji-ścieżek)
- [Wykład 3: Techniki pierwiastkowe](#wykład-3-techniki-pierwiastkowe)
  - [Sumy przedziałów](#sumy-przedziałów)
    - [Podejście brutalne](#podejście-brutalne)
    - [Sumy prefiksowe](#sumy-prefiksowe)
    - [Podejście blokowe](#podejście-blokowe)
      - [Jak wybrać odpowiednie wielkości bloków?](#jak-wybrać-odpowiednie-wielkości-bloków)
    - [Złożoność](#złożoność)
  - [Skoki](#skoki)
    - [Podejście brutalne](#podejście-brutalne-1)
    - [Tablicowanie](#tablicowanie)
    - [Hybrydowe podejście](#hybrydowe-podejście)
    - [Złożoność](#złożoność-1)
  - [Krótka ścieżka Hamiltona](#krótka-ścieżka-hamiltona)
  - [Algorytm Mo](#algorytm-mo)
    - [Optymalizacja przetwarzania zapytań](#optymalizacja-przetwarzania-zapytań)
    - [Analiza złożoności](#analiza-złożoności)
- [Wykład 4: Wyznaczanie minimum na przedziale](#wykład-4-wyznaczanie-minimum-na-przedziale)
  - [Sparse Table](#sparse-table)
    - [Dlaczego przedziały o rozmiarze odpowiadającym potęgom dwójki?](#dlaczego-przedziały-o-rozmiarze-odpowiadającym-potęgom-dwójki)
      - [Jak trzymać coś takiego w pamięci?](#jak-trzymać-coś-takiego-w-pamięci)
    - [Dziel i zwyciężaj](#dziel-i-zwyciężaj)
    - [Kolejka monotoniczna](#kolejka-monotoniczna)
      - [Jaką strukturę warto wybrać, by przechować coś takiego?](#jaką-strukturę-warto-wybrać-by-przechować-coś-takiego)
      - [Podsumujmy złożoność takiego podejścia:](#podsumujmy-złożoność-takiego-podejścia)
- [Wykład 5: Programowanie dynamiczne](#wykład-5-programowanie-dynamiczne)
  - [Idea  programowania dynamicznego](#idea--programowania-dynamicznego)
  - [Szukanie średnicy drzewa](#szukanie-średnicy-drzewa)
  - [Maksymalne skojarzenie na drzewie](#maksymalne-skojarzenie-na-drzewie)
  - [Operatory bitowe](#operatory-bitowe)
    - [Logiczna koniunkcja, alternatywa, alternatywa wykluczająca oraz negacja:](#logiczna-koniunkcja-alternatywa-alternatywa-wykluczająca-oraz-negacja)
  - [Ludzie i maszyny](#ludzie-i-maszyny)
  - [Liczba ścieżek Hamiltona](#liczba-ścieżek-hamiltona)
- [Wykład 6: Drzewa przedziałowe](#wykład-6-drzewa-przedziałowe)
  - [Sumy prefiksowe](#sumy-prefiksowe-1)
    - [Reprezentacja drzewa w pamięci](#reprezentacja-drzewa-w-pamięci)
  - [Inwersje w ciągu](#inwersje-w-ciągu)
    - [Tablica wystąpień](#tablica-wystąpień)
    - [Efektywna implementacja na drzewie przedziałowym](#efektywna-implementacja-na-drzewie-przedziałowym)
    - [Liczenie sumy sufiksowej](#liczenie-sumy-sufiksowej)
  - [Funkcja rekurencyjna](#funkcja-rekurencyjna)
  - [Modyfikacja na przedziale](#modyfikacja-na-przedziale)
  - [Maksymalne prefiksy](#maksymalne-prefiksy)
  - [Maksymalne sumy](#maksymalne-sumy)
    - [Łączenie przedziałów](#łączenie-przedziałów)
- [Wykład 7: Drzewa przedziałowe cd.](#wykład-7-drzewa-przedziałowe-cd)
  - [Pierwsza mniejsza lub równa wartość](#pierwsza-mniejsza-lub-równa-wartość)
  - [Usuwanie z listy](#usuwanie-z-listy)
    - [Przykład](#przykład)
    - [Ogólna metoda](#ogólna-metoda)
  - [Modyfikacja i maksimum](#modyfikacja-i-maksimum)
  - [Ustawianie wartości i maksimum](#ustawianie-wartości-i-maksimum)
      - [Przypadek 1:](#przypadek-1)
      - [Przypadek 2:](#przypadek-2)
    - [Jak działa leniwa propagacja?](#jak-działa-leniwa-propagacja)
    - [Schemat działania algorytmu](#schemat-działania-algorytmu)
- [Wykład 8: Operacje na drzewach](#wykład-8-operacje-na-drzewach)
  - [Binary Lifting](#binary-lifting)
  - [Najniższy wspólny przodek](#najniższy-wspólny-przodek)
  - [Odległość wierzchołków](#odległość-wierzchołków)
  - [Zliczanie ścieżek](#zliczanie-ścieżek)
  - [Sumy w poddrzewach](#sumy-w-poddrzewach)
  - [Sumy na ścieżkach](#sumy-na-ścieżkach)
- [Wykład 9: Grafy z wagami](#wykład-9-grafy-z-wagami)
  - [Algorytm Dijkstry](#algorytm-dijkstry)
  - [Implementacja Algorytmu Dijkstry](#implementacja-algorytmu-dijkstry)
  - [Liczba ścieżek](#liczba-ścieżek)
  - [Najkrótsza ścieżka z kuponem](#najkrótsza-ścieżka-z-kuponem)
  - [Algorytm Bellmana-Forda](#algorytm-bellmana-forda)
  - [Algorytm Floyda-Warshalla](#algorytm-floyda-warshalla)
- [Wykład 10: Grafy](#wykład-10-grafy)
  - [Sortowanie topologiczne](#sortowanie-topologiczne)
    - [Lista do odwiedzenia](#lista-do-odwiedzenia)
    - [DFS](#dfs)
  - [Silnie spójne składowe](#silnie-spójne-składowe)
    - [Pierwsza obserwacja](#pierwsza-obserwacja)
    - [Druga obserwacja](#druga-obserwacja)
    - [Algorytm wyznaczania silnie spójnych składowych](#algorytm-wyznaczania-silnie-spójnych-składowych)
  - [Cykl Eulera](#cykl-eulera)
    - [Jakie grafy mają cykl Eulera?](#jakie-grafy-mają-cykl-eulera)
    - [Zwykły DFS](#zwykły-dfs)
    - [DFS z sortowania topologicznego](#dfs-z-sortowania-topologicznego)
    - [Ścieżka Eulera](#ścieżka-eulera)
    - [Wskazówka implementacyjna](#wskazówka-implementacyjna)
- [Wykład 11: Geometria](#wykład-11-geometria)
  - [Iloczyn wektorowy](#iloczyn-wektorowy)
  - [Pole trójkąta](#pole-trójkąta)
  - [Pole wielokąta](#pole-wielokąta)
  - [Otoczka wypukła](#otoczka-wypukła)
    - [Sortowanie kątowe](#sortowanie-kątowe)
    - [Otoczka górna i dolna](#otoczka-górna-i-dolna)
- [Wykład 12: Haszowanie](#wykład-12-haszowanie)
  - [Funkcja haszująca](#funkcja-haszująca)
    - [Kolizje](#kolizje)
    - [Przykłady](#przykłady)
    - [Prawdopodobieństwo kolizji](#prawdopodobieństwo-kolizji)
    - [Jak wybrać odpowiednie $M$?](#jak-wybrać-odpowiednie-m)
  - [Obliczanie haszy](#obliczanie-haszy)
  - [Porównanie zbiorów liczb](#porównanie-zbiorów-liczb)
  - [Hasze prefiksowe](#hasze-prefiksowe)
  - [Porównanie leksykograficzne](#porównanie-leksykograficzne)
  - [Najmniejszy leksykograficznie sufiks](#najmniejszy-leksykograficznie-sufiks)
  - [Najdłuższy palindrom](#najdłuższy-palindrom)
- [Wykład 13: Algorytmy tekstowe](#wykład-13-algorytmy-tekstowe)
  - [Wyszukiwanie wzorca (KMP)](#wyszukiwanie-wzorca-kmp)
    - [Prefikso-sufiksy](#prefikso-sufiksy)
    - [Standardowe wyszukiwanie](#standardowe-wyszukiwanie)
    - [Trikowe wyszukiwanie](#trikowe-wyszukiwanie)
  - [Drzewa trie](#drzewa-trie)
    - [Wszystkie wystąpienia wzorca](#wszystkie-wystąpienia-wzorca)
    - [Słowa spełniające warunek](#słowa-spełniające-warunek)
- [Wykład 14: Teoria gier](#wykład-14-teoria-gier)
  - [Przykłady gier](#przykłady-gier)
  - [Uogólnienie](#uogólnienie)
  - [Podobieństwo gier](#podobieństwo-gier)



Każdy, kto chce, może uzupełniać poniższe notatki.

# Przydatne w rozwiązywaniu zadań

## Linijki przyspieszające w C++

```plaintext
ios_base::sync_with_stdio(false);
```

Jest to wyłączenie synchronizacji z `stdio`, które pozwala aby bufor oczyszczał się niezależnie od użytkownika. Jest to kiepskie do debugowania, bo możemy dostać wyniki np. dopiero po wpisaniu wszystkich danych na koniec programu. Dla sprawdzaczki jest to jednak nieistotne - sprawdza ona tylko poprawność wyników.

```plaintext
cin.tie(0);
cout.tie(0);
```

Jest to wyłączenie powiązania między `cin` a `cout`. W praktyce chodzi o to, że wszystkie wpisywania i wypisywania zachodzą niezależnie od siebie. Człowiekowi jest łatwiej testować, gdy są powiązane, jednak sprawdzaczce nie robi to różnicy.

Jest konwencja, że linijki przyspieszające wklejami zawsze na początku funkcji `main`:

```plaintext
ios_base::sync_with_stdio(false);
cin.tie(0);
cout.tie(0);
```

Warto również dodać, że użycie `cout << endl;` do wypisania znaku nowej linii automatycznie synchronizuje wyjście, czyli może spowolnić nasz kod. Dlatego lepiej jest używać `cout << '\n';` . To pierwsze może być ewentualnie przydatne przy debugowaniu bądź w programach interaktywnych, których u nas nie będzie.

## Szukanie błędów w kodzie

Aby łatwiej znaleźć błędy wykonania (runtime error) i małe niedopatrzenia, możemy wykorzystać odpowiednie flagi kompilatora. Dokładne wyjaśnienie ich działania znajduje się [tutaj](https://codeforces.com/blog/entry/15547).

```plaintext
-Wall -Wextra -pedantic -std=c++11 -O2 -Wshadow -Wformat=2 -Wfloat-equal -Wconversion -Wlogical-op -Wshift-overflow=2 -Wduplicated-cond -Wcast-qual -Wcast-align -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -D_FORTIFY_SOURCE=2 -fsanitize=address -fsanitize=undefined -fno-sanitize-recover -fstack-protector
```

Pozwala to wyłapać błędy takie jak:
* wychodzenie poza indeksy,
* niepotrzebne zmienne,
* użycie niezainicjowanych zmiennych.

## Duże tablice

Warto deklarować duże tablice jako globalne. Znajdują się w innym miejscu w pamięci i od razu są inicjalizowane zerami.

# Wykład 1: Teoria liczb

Omówiony materiał:

* Rozszerzony Algorytm Euklidesa,
* znajdowanie odwrotności modularnej za pomocą Rozszerzonego Algorytmu Euklidesa oraz Małego Twierdzenia Fermata,
* krótka powtórka z Sita Eratostenesa,
* zliczanie dzielników liczb,
* funkcja Eulera, na razie bez dowodu.

### Rozszerzony algorytm Euklidesa

:::success
Zadanie. Dla danych liczb $a,b$ znajdź takie $k, l$, że $ak + bl = nwd(a,b)$.
:::

Użyjemy zmodyfikowanego algorytmu Euklidesa. Będziemy zwracać oprócz nwd parę liczb $k$, $l$.

#### Wywołanie $nwd(a,0)$

$$ nwd(a,0) = a = a \cdot 1 + 0 \cdot 0 $$
Zwracamy $k=1$, $l=0$.

#### Wywołanie $nwd(b,a\%b)$

Algorytm zwrócił $d=nwd(b,a\%b)=nwd(a,b)$ oraz parę $(k,l)$ taką, że $bk+(a\%b)l=d$

Skorzystamy z faktu, że

$$ a\%b = a - \left\lfloor\frac{a}{b}\right\rfloor b $$

Zatem:

$$ bk +  \left( a - \left\lfloor\frac{a}{b}\right\rfloor b \right) l = d $$
$$ bk + al - \left\lfloor\frac{a}{b}\right\rfloor bl = d $$
$$ al + b \left( k - \left\lfloor\frac{a}{b}\right\rfloor l \right) = d $$

Z tego wywołania możemy zwrócić więc $d$ oraz parę $(l, k - \lfloor\frac{a}{b}\rfloor l)$


### Znajdowanie odwrotności modularnej

Dla pewnych liczb $a, m$ szukamy $a^{-1}$ t. ż:

$$ aa^{-1} \equiv 1 \mod m $$

Nie dla każdego $a$ i $m$ taka odwrotność istnieje. Gwarancję mamy tylko wtedy, gdy $a$ jest względnie pierwsze z $m$. 

#### Metoda 1: Rozszerzony algorytm Euklidesa

Rozszerzony algorytm euklidesa dla $a$ i $m$ zwróci $k$ i $l$ takie, że:

$$ ak + ml = nwd(a,m) = 1 $$

Zatem 

$$ ak + ml \equiv 1 \mod m $$
$$ ak \equiv 1 \mod m $$
$$ k \equiv a^{-1} \mod m $$

Zatem pierwszą liczbą z pary zwróconej przez rozszerzony algorytm Euklidesa jest szukana odwrotność.

#### Metoda 2: Małe twierdzenie Fermata

**W tym przypadku będziemy zakładać, że $m$ jest pierwsze.**

:::info
Twierdzenie. Jeśli $p$ jest liczbą pierwszą względnie pierwszą z pewną liczbą $a$, to zachodzi:

$$ a^{p-1} \equiv 1 \mod p $$
:::

Zatem

$$ a \cdot a^{m-2} \equiv 1 \mod m $$

Więc $a^{m-2}$ jest odwrotnością $a$ modulo $m$.

### Funkcja Eulera

:::info
Definicja. Funkcja Eulera $\phi(n)$ jest równa liczbie liczb ze zbioru $\{1, 2, \dots, n\}$, które są względnie pierwsze z $n$.
:::

Przykłady:

* $\phi(7) = |\{1, 2, 3, 4, 5, 6\}| = 6$,
* $\phi(12) = |\{1, 5, 7, 11\}| = 4$.

#### Jeśli $p$ jest pierwsza, to $\phi(p)=p-1$.

Uzasadnienie: wszystkie liczby między $1$ a $p-1$ włącznie są względnie pierwsze z $p$.

####  Jeśli $p$ jest pierwsza, to $\phi(p^k)=p^k-p^{k-1}=p^k(1-\frac{1}{p})$.

Uzasadnienie: między $1$ a $p^k$ co $p$-ta liczba dzieli się przez $p$, zatem pozostałych liczb jest $p^k - \frac{p^k}{p} = p^k-p^{k-1}$.

# Wykład 2: Union Find


### Dowód własności funkcji Eulera

#### Jeśli $a,b$ są względnie pierwsze, to $\phi(ab)=\phi(a)\phi(b)$

Dowód:

Rozważmy dowolny $1 \leq x \leq ab$. Wtedy

$$NWD(x,ab)=1 \iff NWD(x,a)=1 \land NWD(x,b)=1$$

Szukamy więc takich $x$, które spełniają prawą stronę powyższej równoważności, a więc takich, że
$$
\begin{cases}
x \equiv k \text{ (mod } a) \\
x \equiv l \text{ (mod } b)
\end{cases}
$$
Gdzie $k$ jest względnie pierwsze z $a$, a $l$ jest względnie pierwsze z $b$. Takich $k$ jest dokładnie $\phi(a)$, a $l$ dokładnie $\phi(l)$. Na mocy Chińskiego Twierdzenia o Resztach, dla każdej pary $(k,l)$, taki $x$ jest określony dokładnie jeden, zatem wszystkich takich $x$ jest tyle, co par $(k, l)$, czyli $\phi(a)\phi(b)$.

### Chińskie twierdzenie o resztach
    
:::success
Zadanie. Dane jest $N$ względnie pierwszych liczb $p_1, \dots, p_N$ oraz liczby $a_1, \dots, a_N$, takie że $0 \leq a_i < p_i$. 
Znajdź najmniejszą nieujemną liczbę $x$ spełniającą warunki:
	
\begin{align*}
x \text{ mod } p_1 &= a_1, \\
x \text{ mod } p_2 &= a_2, \\
\dots & \\
x \text{ mod } p_N &= a_N. \\
\end{align*}
:::
Ze względu, że liczby $p_i$ są względnie pierwsze, wszystkimi odpowiedziami na ten układ równań są liczby postaci $x + k \cdot p_1 \cdot \ldots \cdot p_N$, dla dowolnego całkowitego $k$.
	
#### Rozwiązanie
	
Niech $M = p_1 \cdot \ldots \cdot p_N$. 
	Naszym celem będzie znalezienie takich wartości $w_1\ \ldots, w_N$, że dla każdego $1 \le i \le N$ będą spełnione własności:
    
* $w_i \text{ mod } p_i = a_i$,
* $w_i \text{ mod } p_j = 0$, dla $i \neq j$. 
	
Wtedy naszym rozwiązaniem będzie po prostu $x = (w_1 + \ldots + w_N) \text{ mod } M$, co łatwo zweryfikować.
	
Aby spełniona była druga własność, każdy element $w_i$ możemy pomnożyć przez $m_i = M/p_i$ -- wtedy będzie dawał on wynik $0$ modulo wszystkie $p_j$, takie, że $i \neq j$.
Zauważmy, że jeśli pomnożymy teraz $m_i$ razy $r_i = m_i^{-1} \text{ mod } p_i$, uzyskamy wartość $1$ modulo $p_i$.
Zatem ten iloczyn wystarczy nam pomnożyć przez $a_i$.
	
Podsumowując, aby obliczyć wynik musimy wyliczyć kolejno:
* $M = p_1 \cdot \ldots \cdot p_N$,
* $m_i = M/p_i$,
* $r_i = m_i^{-1} \text{ mod } p_i$,
* $w_i = m_i \cdot r_i \cdot a_i$ mod $M$,
* $x = (w_1 + \ldots + w_N)$ mod $M$.
	
### Problem struktury zbiorów rozłącznych
#### Technika łączenia mniejszego zbioru do większego
#### Union Find

Sensowne informacje na temat struktury można znaleźć na przykład [tutaj](https://cp-algorithms.com/data_structures/disjoint_set_union.html).

#### Technika kompresji ścieżek.


# Wykład 3: Techniki pierwiastkowe

Przydatne informacje można znaleźć na przykład [tutaj](https://cp-algorithms.com/data_structures/sqrt_decomposition.html).

## Sumy przedziałów

Zajmiemy się problemem optymalnego obliczania sumy z przedziału w tablicy. Załóżmy, że mamy dwie operacje:

- `policz(a,b)` - policz sumę od indeksu `a` do `b`,
- `zamien(k,u)` - zamień wartość o indeksie `k` na `u`.

### Podejście brutalne

Przy operacji `policz(a,b)` można po prostu dodać kolejne elementy tablicy, co w najgorszym przypadku zajmuje nam **O(n)**. Natomiast zamiana wartości jest bardzo szybka — **O(1)**.

### Sumy prefiksowe

Innym sposobem jest algorytm sum prefiksowych. Przykładowa tablica `T` i jej sumy prefiksowe `S`:

| `i`   | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 |
|-------|---|---|---|---|---|---|---|---|---|----|----|----|
| `T[i]` | 1 | 3 | 4 | 5 | 2 | 9 | 4 | 8 | 3 |  1 |  2 |  5 |
| `S[i]` | 1 | 4 | 8 | 13| 15| 24| 28| 36| 39| 40 | 42 | 45 |

Wtedy `policz(a,b)` to po prostu **`S[b] - S[a-1]`** w czasie **O(1)**. Niestety, `zamien` staje się kosztowne, bo wymaga przeliczenia sum prefiksowych, co zajmuje **O(n)**.

### Podejście blokowe

Załóżmy, że znamy sumę pewnej części tablicy, np. od indeksu 4 do 9:

| `i`   | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 |
|-------|---|---|---|---|---|---|---|---|---|----|----|----|
| `T[i]` | 1 | 3 | 4 | 5 | 2 | 9 | 4 | 8 | 3 |  1 |  2 |  5 |
| `Blok` | ? | ? | ? | 31| 31| 31| 31| 31| 31|  ? |  ? |  ? |

Aby dostać się do sumy, wewnątrz bloku, wystarczy usunąć odpowiednie wartości tablicy, analogicznie spoza bloku — dodać to, czego brakuje. Problemem tego rozwiązania są sytuacje, w których trzeba liczyć dużo spoza części wspólnej. Można to łatwo rozwiązać — znać sumę wielu bloków.

#### Jak wybrać odpowiednie wielkości bloków?

Podzielmy całą tablicę na bloki o pewnej długości $B$. Dla każdego bloku będziemy trzymali jego sumę, co zaoszczędzi nam przechodzenia kolejno po wszystkich elementach wewnątrz bloku.

Złożoność odpowiedzi na zapytanie to suma trzech czynników:

- ilości elementów przed blokami (w najgorszym przypadku $B-1$),
- ilości bloków (w najgorszym przypadku wszystkie bloki),
- ilości elementów po blokach (w najgorszym przypadku $B-1$).

Rozważmy przypadki:

- **Małe bloki** — nie liczy się wiele szybciej niż w rozwiązaniu brutalnym, bo musimy policzyć sumę wielu bloków.
- **Duże bloki** — dużo się liczy spoza części policzonej w blokach.



W sumie:
$$
B + \frac{N}{B} + B = O \left( B + \frac{N}{B} \right)
$$

Wystarczy wybrać takie $B$, że:

$$
B = \frac{N}{B}
$$

Zatem najbardziej optymalny rozmiar to:

$$
B = \sqrt{N}
$$

Nasza złożoność sprowadza się wtedy do $O(\sqrt{N})$.

Jest jeszcze jeden sposób na rozwiązanie tego zadania — drzewo przedziałowe, jednak zajmiemy się nim za trzy tygodnie.

### Złożoność

| Metoda               | `zamien(u, k)` | `policz(a, b)` |
|----------------------|----------------|----------------|
| Brutalne             | $O(1)$         | $O(n)$         |
| Sumy prefiksowe      | $O(n)$         | $O(1)$         |
| Bloki                | $O(1)$         | $O(\sqrt{n})$  |
| Drzewo przedziałowe  | $O(\log{n})$   | $O(\log{n})$   |

## Skoki

Rozważmy problem obliczenia sumy postaci:

$$
S = T[a] + T[a+b] + T[a+2 \cdot b] + \dots
$$

### Podejście brutalne

Możemy policzyć tę sumę wprost z definicji, co oczywiście ma złożoność $O(n)$, ponieważ w najgorszym przypadku ($a = 0$ oraz $b = 1$) musimy zsumować całą tablicę.

- ✅ Minimalne zużycie pamięci — potrzebujemy tylko miejsca na tablicę $T$.
- ✅ Szybka obsługa zapytań z dużymi skokami.
- ❌ Wolne działanie dla małych skoków.

### Tablicowanie

Z drugiej strony, jeśli mamy wiele zapytań do jednej wartości $b$, warto rozważyć **preprocessing**. Po wstępnym przetworzeniu danych każde zapytanie obsługujemy błyskawicznie, w czasie $O(1)$. Niestety, kosztem jest ogromna złożoność pamięciowa — $O(n^2)$, co dla dużych tablic staje się niepraktyczne.

Dla ilustracji rozważmy przykład tablicowanych sum:

| $i$ | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
|---------|---|---|---|---|---|---|---|---|---|----|
| $T[i]$ | 1 | 7 | 2 | 3 | 5 | 4 | 3 | 1 | 5 | 4 |
| $b = 1$ | 35 | 34 | 27 | 25 | 22 | 17 | 13 | 10 | 9 | 4 |
| $b = 2$ | 16 | 19 | 15 | 12 | 13 | 9 | 8 | 5 | 5 | 4 |

- ✅ Błyskawiczna obsługa każdego zapytania.
- ❌ Ogromne zapotrzebowanie na pamięć $O(n^2)$.
- ❌ Marnowanie zasobów dla dużych skoków.

### Hybrydowe podejście

Spróbujmy wykorzystać zalety obu metod. **Preprocessing** przy małych skokach, gdy brutalne rozwiązanie liczy się długo i **obliczania brutalnego** przy dużych skokach, gdy trzymanie danych w tablicy jest nieopłacalne. Pozostaje pytanie -- jakie dane są odpowiednio małe?

Tak, jak wcześniej, odpowiednią równowagę zachowujemy, gdy tablicujemy wyniki do $\sqrt{n}$.

Możemy to zaimplementować zarówno poprzez **leniwe tablicowanie** (obliczenia na żądanie z zapamiętywaniem), jak i przez **preprocessing** dla wartości $b$ od 1 do $\sqrt{n}$.

### Złożoność

| Metoda              | Złożoność zapytania | Złożoność preprocessingu |
|---------------------|---------------------|--------------------------|
| **Brutalna siła**   | $O(n)$          | $-$                 |
| **Pełne tablicowanie** | $O(1)$          | $O(n^2)$            |
| **Hybryda**         | $O(\sqrt{n})$   | $O(n\sqrt{n})$      |
| **Leniwa hybryda** | $O(\sqrt{n})$ | $O(n\sqrt{n})$      |

W ten sposób otrzymujemy uniwersalne rozwiązanie, które działa dobrze zarówno dla pojedynczych zapytań, jak i dla wielu zapytań wykonywanych na dużej tablicy.

## Krótka ścieżka Hamiltona

Rozważmy problem planszy $n \times n$, na której rozmieszczonych jest $n$ punktów o współrzędnych $p_i = (x_i, y_i)$. Naszym zadaniem będzie wybrać teraz taką permutację punktów $p_0, p_1, \dots, p_n$, aby:

$$
\sum_{i=1}^{n-1} |x_{i+1} - x_i| + |y_{i+1} - y_i| \leq n\sqrt{n}
$$

Można spróbować sortować punkty po współrzędnej $x$. Jednak bardzo łatwo znaleźć przykład, w którym punkty są blisko siebie na osi $OX$, a jednak ich druga współrzędna bardzo się zmienia. Wtedy możemy już w kilku punktach wyjść poza oczekiwany limit $n\sqrt{n}$.

Innym podejściem jest podzielenie planszy na $\sqrt{n} \cdot \sqrt{n}$ bloków o rozmiarze $\sqrt{n}$, a wewnątrz każdego bloku sortowanie punktów po współrzędnej $x$. Teraz należy wybrać odpowiednią kolejność odwiedzania bloków. Łatwo zauważyć, że najbardziej optymalnie jest przechodzić wężykiem po tablicy. Zobaczmy przykład $4 \times 4$ takiej kolejności:

$$
\begin{matrix}
16 & 15 & 14 & 13 \\
9  & 10 & 11 & 12 \\
8  & 7  & 6  & 5  \\
1  & 2  & 3  & 4 
\end{matrix}
$$

## Algorytm Mo

Załóżmy, że mamy tablicę z kolorami.

| $i$    | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
| -------- | - | - | - | - | - | - | - | - | - |
| $T[i]$ | 1 | 2 | 3 | 2 | 3 | 2 | 1 | 1 | 2 |

Naszym zadaniem będzie odpowiadać na pytanie, ile różnych kolorów jest na zadanym przedziale.

Przyjmijmy, że znamy ilość kolorów w pewnym przedziale tablicy. Rozszerzenie bądź zwężenie tego przedziału o jedną komórkę nie jest trudne, ponieważ jeśli pojawia się nowy kolor, zwiększamy licznik i odnotowujemy jego wystąpienie w jakiejś tablicy, a jeśli usuniemy wszystkie wystąpienia – zmniejszamy licznik.

### Optymalizacja przetwarzania zapytań

Jakie sytuacje są dla nas problematyczne w tej metodzie? Weźmy taki ciąg zapytań:

- (1,6)
- (5,6)
- (1,7)

Taki ciąg zostałby przetworzony jako:

1. obliczenie wyniku dla (1,6),
2. usunięcie danych do 5,
3. ponowne obliczenie wyniku dla (1,7).

Istnieje jednak pewna analogia między sprytnym układaniem punktów na planszy w "krótkiej" ścieżce Hamiltona a efektywnym sortowaniem zapytań w Algorytmie Mo.

Wyobraźmy sobie, że każdy przedział $[p, k]$ reprezentujemy na płaszczyźnie jako punkt o współrzędnych $(p, k)$. Tak, jak wcześniej wystarczy przechodzić wężem. Sortujemy punkty w pierwszej kolejności po przynależności do bloków o rozmiarze $\sqrt{N}$, a następnie w zależności od parzystości bloku zwiększamy jego rozmiar lub zmniejszamy, co można przyrównać do chodzenia góra-dół po planszy.

### Analiza złożoności

- Prawy koniec przedziału możemy przsunąć maksymalnie o $N$ wewnątrz każdego poziomego rzędu bloków → $N\sqrt{N}$.
- Lewy koniec przedziału możemy przesunąć o $\sqrt{N}$ na każde zapytanie wewnątrz jednego rzędu bloków → $Q\sqrt{N}$.
- Przesunięcia lewego przedziału w sytuacjach, gdy przeskakujemy między rzędami bloków kosztują nas sumarycznie $N$.
- Sortowanie zapytań → $Q\log{Q}$.

Co w sumie daje nam:

$$
O(N\sqrt{N} + Q\sqrt{N}) = O((N+Q)\sqrt{N})
$$

# Wykład 4: Wyznaczanie minimum na przedziale

## Sparse Table

Przydatne informacje można znaleźć na przykład [tutaj](https://cp-algorithms.com/data_structures/sparse-table.html).

We wcześniejszych algorytmach z dzieleniem tablicy na bloki o rozmiarze $\sqrt{N}$ wyznaczaliśmy sumy i je modyfikowaliśmy w zależności od tego, czego wymagało zapytanie. Jednak z wyznaczaniem minimum jest pewien problem, w szczególności ze zmniejszaniem zakresu.

Rozważmy przykład:

| $T[i]$ | 1 | 6 | 2 |
|----------|---|---|---|
| $\min(1,3)$ | 1 | 1 | 1 |

Gdybyśmy chcieli pozbyć się z bloku minimalnej liczby 1, nie wiadomo, która jest mniejsza w kontekście całego bloku. Trzeba przejrzeć i zaktualizować dane, by dowiedzieć się, że to 2. W problemie sumy wystarczyło tylko odjąć starą wartość i dodać nową.

Spróbujmy sprytnie podzielić tablicę na podprzedziały, z których łatwo i szybko można złożyć zadany przedział.

<table class="tg"><thead>
  <tr>
    <th class="tg-0pky">i</th>
    <th class="tg-0lax">1</th>
    <th class="tg-0lax">2</th>
    <th class="tg-0lax">3</th>
    <th class="tg-0lax">4</th>
    <th class="tg-0lax">5</th>
    <th class="tg-0lax">6</th>
    <th class="tg-0lax">7</th>
    <th class="tg-0lax">8</th>
    <th class="tg-0lax">9</th>
    <th class="tg-0lax">10</th>
    <th class="tg-0lax">11</th>
  </tr></thead>
<tbody>
  <tr>
    <td class="tg-0lax">T[i]</td>
    <td class="tg-0lax">1</td>
    <td class="tg-0lax">7</td>
    <td class="tg-0lax">2</td>
    <td class="tg-0lax">3</td>
    <td class="tg-0lax">1</td>
    <td class="tg-0lax">9</td>
    <td class="tg-0lax">4</td>
    <td class="tg-0lax">5</td>
    <td class="tg-0lax">7</td>
    <td class="tg-0lax">3</td>
    <td class="tg-0lax">2</td>
  </tr>
  <tr>
    <td class="tg-0lax" rowspan="2">min(T[i],T[i+1])</td>
    <td class="tg-0lax" colspan="2">1</td>
    <td class="tg-0lax" colspan="2">2</td>
    <td class="tg-0lax" colspan="2">1</td>
    <td class="tg-0lax" colspan="2">4</td>
    <td class="tg-0lax" colspan="2">3</td>
    <td class="tg-0lax"></td>
  </tr>
  <tr>
    <td class="tg-0lax"></td>
    <td class="tg-0lax" colspan="2">2</td>
    <td class="tg-0lax" colspan="2">1</td>
    <td class="tg-0lax" colspan="2">4</td>
    <td class="tg-0lax" colspan="2">5</td>
    <td class="tg-0lax" colspan="2">2</td>
  </tr>
  <tr>
    <td class="tg-baqh" rowspan="4">min(T[i],T[i+1],T[i+2],T[i+3])</td>
    <td class="tg-0lax" colspan="4">1</td>
    <td class="tg-0lax" colspan="4">1</td>
    <td class="tg-0lax" colspan="3">3</td>
  </tr>
  <tr>
    <td class="tg-0lax"></td>
    <td class="tg-0lax" colspan="4">1</td>
    <td class="tg-0lax" colspan="4">4</td>
    <td class="tg-0lax" colspan="2"></td>
  </tr>
  <tr>
    <td class="tg-0lax" colspan="2"></td>
    <td class="tg-0lax" colspan="4">1</td>
    <td class="tg-0lax" colspan="4">3</td>
    <td class="tg-0lax"></td>
  </tr>
  <tr>
    <td class="tg-0lax" colspan="3"></td>
    <td class="tg-0lax" colspan="4">1</td>
    <td class="tg-0lax" colspan="4">2</td>
  </tr>
  <tr>
    <td class="tg-0lax" rowspan="4">min(T[i],...,T[i+7])</td>
    <td class="tg-0lax" colspan="8">1</td>
    <td class="tg-0lax" colspan="3"></td>
  </tr>
  <tr>
    <td class="tg-0lax"></td>
    <td class="tg-0lax" colspan="8">1</td>
    <td class="tg-0lax" colspan="2"></td>
  </tr>
  <tr>
    <td class="tg-0lax" colspan="2"></td>
    <td class="tg-0lax" colspan="8">1</td>
    <td class="tg-0lax"></td>
  </tr>
  <tr>
    <td class="tg-0lax" colspan="3"></td>
    <td class="tg-0lax" colspan="8">1</td>
  </tr>
</tbody></table>

### Dlaczego przedziały o rozmiarze odpowiadającym potęgom dwójki?

Wiadomo, że z nich da się złożyć każdą liczbę, zatem ten podział pozwoli nam znacznie przyspieszyć odpowiadanie na pytania.

#### Jak trzymać coś takiego w pamięci?

Wystarczy mieć tablicę dwuwymiarową $T[N][log N]$. Wtedy $T[i][j]$ będzie trzymać informację o minimum na przedziale $[i, i + 2^j - 1]$.

Na przykładzie $[a, a+10]$ wystarczy wyznaczyć minimum z:
- $T[a][3]$, czyli $\min(a, a+7)$,
- $T[a+8][1]$, czyli $\min(a+8, a+9)$,
- $T[a+10][0]$, czyli $T[a+10]$.

Taką tablicę można wypełnić budując większe przedziały na podstawie mniejszych, np. dla czterech wystarczy wybrać mniejsze z dwójek:

$$\min(T[i],...,T[i+3])=\min(\min(T[i],T[i+1]),\min(T[i+2],T[i+3]))$$
	
Rozkładając liczbę na potęgi dwójki dostęp do odpowiedzi na zapytanie jest oczywiście $O(\log{N})$. Zauważając jednak, że $\min(a,a)=a$, więc możemy obliczyć np. $\min[a,a+10]$ w $O(1)$ za pomocą dwóch ósemek:

$$\min[a,a+10]=\min([a,a+7],[a+3,a+10])$$

### Dziel i zwyciężaj

Przydatne informacje można znaleźć na przykład [tutaj](https://usaco.guide/plat/DC-SRQ?lang=cpp).

Zaczniemy od pewnej obserwacji.

Załóżmy, że mamy tablicę, w której musimy policzyć minimum dla przedziałów, gdzie wszystkie zaczynają się od indeksu pierwszego. Wtedy można wyznaczać minimum tak, jak w sumach prefiksowych.

| $i$      | 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  | 9  | 10 | 11 | 12 |
|----------|----|----|----|----|----|----|----|----|----|----|----|----|
| $T[i]$   | 7  | 8  | 5  | 6  | 9  | 4  | 3  | 7  | 8  | 9  | 2  | 1  |
| $pref[i]$| 7  | 7  | 5  | 5  | 5  | 4  | 3  | 3  | 3  | 3  | 2  | 1  |

Rozważmy teraz metodę, która zapamięta wszystkie zapytania (algorytm offline). Będziemy działać podobnie do Merge Sort. Dzielimy tablicę na pół, w punkcie $s$, a wraz z nią segregujemy zapytania na:

- **lewe** - te, które są na lewo od punktu $s$, $a \leq s$ oraz $b \leq s$,
- **środkowe** - te, dla których $a \leq s$ oraz $b > s$,
- **prawe** - te, które są na prawo od punktu $s$, $a > s$ oraz $b > s$.

Podczas rekurencji lewe i prawe przedziały zostawimy do obliczenia rekurencyjnego, podobnie jak w Merge Sorcie, wywołując się na lewej i prawej części. Natomiast każdy środkowy przedział $[p, k]$ podzielimy na dwa przedziały: $[p, s]$ oraz $[s+1, k]$, po czym policzymy korzystając z wcześniejszej obserwacji – będziemy mieć do policzenia minima od jednego punktu.

Na przykładzie wygląda to tak:

| $i$      | 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  | 9  |
|------------|----|----|----|----|----|----|----|----|----|
| $T[i]$   | 2  | 9  | 8  | 5  | 7  | 5  | 4  | 9  | 7  |
| $Lewe[i]$| 2  | 5  | 5  | 5  |    |    |    |    |    |
| $Prawe[i]$|    |    |    |    | 7  | 5  | 4  | 4  | 4  |


W rekurencji musimy jedynie przekazywać zapytania, które rekurencja obliczy później, czyli lewe i prawe części, które wcześniej zostały pominięte. Wygodnie jest je trzymać jako wektory par.

Ile razy wywoła się taki program? Kolejna analogia do Merge Sort – $O(N \log N)$. Weźmy przykład ośmioelementowej tablicy i zapytań o pojedyncze elementy.

```plaintext
                                     [1, 8]
                                    /      \
                              [1, 4]       [5, 8]
                             /     \       /     \
                        [1, 2]  [3, 4]   [5, 6] [7, 8]
                       /   \    /   \   /    \   /   \
                     [1]  [2] [3]  [4] [5]  [6] [7]  [8]
```

Suma wywołań (długości przedziałów) na każdym poziomie rekurencji jest równa $N$, a poziomów jest w najgorszym przypadku $\log N$, stąd złożoność $O(N \log N)$.

### Kolejka monotoniczna

Przydatne informacje można znaleźć na przykład [tutaj](https://cp-algorithms.com/data_structures/stack_queue_modification.html).

Kolejny pomysł na rozwiązanie problemu minimum na przedziałach również wykorzystuje wcześniejszą obserwację.

Weźmy tablicę i posortujmy zapytania tak, aby wszystkie kończyły się w jednym miejscu. Policzymy minima sufiksowe od początku tablicy do punktu, w którym kończy się aktualnie przeglądana grupa zapytań. Zauważmy, że rozszerzenie takiego przedziału nie jest trudne - nadpisujemy tylko niektóre elementy. Na przykładzie:

| $i$  | 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  | 9  | 10 | 11 |
|--------|----|----|----|----|----|----|----|----|----|----|----|
| $T[i]$ | 1  | 6  | 3  | 5  | 9  | 8  | 7  | 4  | 8  | 2  | 0  |
| $pref[1,6]$  | 1  | 3  | 3  | 5  | 8  | 8  |    |    |    |    |    |
| $pref[1,7]$  | 1  | 3  | 3  | 5  | 7  | 7  | 7  |    |    |    |    |
| $pref[1,8]$  | 1  | 3  | 3  | 4  | 4  | 4  | 4  | 4  |    |    |    |
| $pref[1,9]$  | 1  | 3  | 3  | 4  | 4  | 4  | 4  | 4  | 8  |    |    |
| $pref[1,10]$ | 1  | 2  | 2  | 2  | 2  | 2  | 2  | 2  | 2  | 2  |    |
| $pref[1,11]$ | 0  | 0  | 0  | 0  | 0  | 0  | 0  | 0  | 0  | 0  | 0  |

Możemy wtedy od razu odpowiadać na zapytania dla pewnej grupy przedziałów. Mając $pref[1,6]$ potrafimy obsłużyć wszystkie zapytania, które kończą się w 6. Zatem jeśli posortujemy zapytania w uporządkowany sposób, będziemy mogli znaleźć wszystkie wyniki za pomocą powyższych tablic. 

#### Jaką strukturę warto wybrać, by przechować coś takiego?
- **Tablica** – oczywiście przechowa dane, ale będziemy marnować zasoby na powtórzenia.
- **Kolejka/stos/wektor par <wartość, indeks_początkowy>** – zapiszemy tylko to, co się zmienia.

Dla powyższego przykładu kolejka wyglądałaby tak:

| Przedział    | Kolejka                          |
|-------------|---------------------------------|
| $pref[1,6]$  | $\{(1,1), (3,2), (5,5), (8,6)\}$ |
| $pref[1,7]$  | $\{(1,1), (3,2), (5,5), (7,6)\}$ |
| $pref[1,8]$  | $\{(1,1), (3,2), (4,5)\}$        |
| $pref[1,9]$  | $\{(1,1), (3,2), (4,5), (8,10)\}$ |
| $pref[1,10]$ | $\{(1,1), (2,2)\}$               |
| $pref[1,11]$ | $\{(0,0)\}$                      |

Z kolejki wynik możemy wyjąć logarytmicznie szybko, stosując przeszukiwanie binarne. Wystarczy znaleźć pierwszy indeks większy lub równy niż szukany i wypisać jego wartość.

#### Podsumujmy złożoność takiego podejścia:
- Wskaźnik prawego  końca przedziału zmieni się maksymalnie $N$ razy (przesunie się przez całą tablicę).
- Każdy dodany element może być co najwyżej raz zdjęty z kolejki.
- Dla każdego zapytania szukamy w kolejce odpowiedniej odpowiedzi $Q \cdot \log N$.

Finalnie otrzymujemy $O(N+Q\log N)$.

# Wykład 5: Programowanie dynamiczne

## Idea  programowania dynamicznego

Zacznijmy od wzoru na **dwumian Newtona**:

$$
{n \choose k} = \frac{n!}{k!\cdot (n-k)!}
$$

Załóżmy, że mamy policzyć dwumian Newtona dla zadanego $n$, $k$. Oczywiście można to liczyć z definicji, jednak liczenie silni może szybko doprowadzić do absurdalnie dużych liczb, które nie zmieszczą się nawet w 64-bitowych zmiennych. 

Skorzystamy z własności dwumianu Newtona:

$$
{n \choose k} = {n-1 \choose k} + {n-1 \choose k-1}
$$

$$
{n \choose 0} = 1
$$

To oznacza, że zamiast obliczać całość od zera, możemy **wykorzystać już policzone wartości** i budować wynik krok po kroku.

Skoro każdy wynik zależy od poprzednich, możemy zapisać je w **tabeli**:

| $n \backslash k$ | 0 | 1 | 2 | 3 | 4 | 5 |
|----------------------|---|---|---|---|---|---|
| **0** | 1 |   |   |   |   |   |
| **1** | 1 | 1 |   |   |   |   |
| **2** | 1 | 2 | 1 |   |   |   |
| **3** | 1 | 3 | 3 | 1 |   |   |
| **4** | 1 | 4 | 6 | 4 | 1 |   |
| **5** | 1 | 5 | 10 | 10 | 5 | 1 |

Każda liczba w tabeli to suma dwóch liczb **nad nią**.  
Można to zobaczyć np. w **wierszu 4, kolumna 2**:  
$6 = 3 + 3$.

W programowaniu dynamicznym zapisujemy wyniki w tablicy (zwykle nazywanej $\text{DP}$) i liczymy wartości w sprytny sposób:

- $\text{DP[n][k]} = {n \choose k}$
- $\text{DP[n][k]} = \text{DP[n-1][k-1]} + \text{DP[n-1][k]}$
- $\text{DP[n][0]} = 1$

Ogólnie rzecz biorąc, idea programowania dynamicznego polega na:

* zdefiniowaniu *stanu* dla pewnych parametrów programu, który będzie opisywał pewną (optymalną) wartość (w powyższym przykładzie stanem dla liczb $n$ i $k$ jest wartość ${n \choose k}$, trzymana w tablicy $DP[n][k]$),
* zdefiniowaniu *metody obliczania stanu* za pomocą wcześniej policzonych stanów (w powyższym przypadku $DP[n][k] = DP[n-1][k-1] + DP[n-1][k]$).

Oczywiście stany powinny być dobrane w taki sposób, żeby finalnie dało się z nich  wyczytać szukaną wartość (w powyższym, możemy znaleźć wartość $n \choose k$ po prostu w komórce $DP[n][k]$).

---

## Szukanie średnicy drzewa

:::info
**Średnicą drzewa** nazywamy najdłuższą ścieżkę w grafie.
:::

:::success
Zadanie. Policz długość średnicy podanego drzewa o $N$ wierzchołkach. 
:::

Aby to zrobić znowu zapamiętamy wyniki dla mniejszych poddrzew, by zbudować z nich wyniki dla większych.

Dla każdego ukorzenionego drzewa (jeśli drzewo nie jest ukorzenione, możemy wybrać korzeń dowolnie) mamy dwie możliwości średnicy:

- **Przechodzi** przez korzeń
- **Nie przechodzi** przez korzeń

Jeśli średnica **przechodzi przez korzeń**, to jej końce muszą znajdować się w dwóch najwyższych poddrzewach.  

Na przykładzie wyglądać może to tak:

![image](https://hackmd.io/_uploads/HJyi7hG6yl.png)
  
Wtedy suma wysokości poddrzew $W2 + W4 + 2$ jest większa od wszystkich średnic w poddrzewach $SR2, SR3, SR4$.

Jeśli średnica **nie przechodzi przez korzeń**, to największa średnica znajduje się w jednym z poddrzew i nie ulega zmianie.

Wyglądałoby to mniej więcej tak:

![image](https://hackmd.io/_uploads/H1ijGhza1l.png)

Wtedy każda suma wysokości dwóch poddrzew jest mniejsza, od średnicy $SR2$, więc ona dalej będzie średnicą naszego drzewa.

Zatem do naszego $\text{DP}$ potrzebujemy dwóch wartości:  
- $\text{MAX_SR}$ - największa średnica znalezionego poddrzewa,  
- $\text{W}$ - wysokość poddrzewa.  

W kodzie wyglądałoby to następująco:

$$
\text{DP}[v] = (\text{MAX_SR}, W)
$$

gdzie:

$\text{MAX_SR} = \max(\max (\text{srednice_poddrzew}), \text{dwie_najwieksze_wysokosci} + 2),$

$W = \max(\text{wysokości poddrzew}) + 1.$

Złożoność takiego rozwiązania to $O(n)$, ponieważ wywołamy się na każdym wierzchołku i dla niego policzymy to, co trzeba.

## Maksymalne skojarzenie na drzewie

:::info
**Skojarzenie** to zbiór krawędzi, gdzie żadna para krawędzi nie łączy tego samego wierzchołka. 
:::
Zobaczmy to na przykładzie:

![image](https://hackmd.io/_uploads/HJPMOpz6kg.png)

Na grafach, **maksymalne skojarzenie** oznacza takie skojarzenie, które zawiera jak najwięcej krawędzi, oczywiście przy zachowaniu warunku, że żadna krawędź nie łączy tego samego wierzchołka dwa razy. Zobaczmy przykłady:

![image](https://hackmd.io/_uploads/HJTtOafaJe.png)



:::success
Zadanie. Policz liczbę krawędzi w największym skojarzeniu w podanym drzewie o $N$ wierzchołkach. 
:::

Podobnie jak w poprzednich zadaniach, możemy obliczać **maksymalne skojarzenie** na drzewie, składając wyniki z poddrzew.

Znowu mamy dwa przypadki:
1. Korzeń **jest** skojarzony.
2. Korzeń **nie jest** skojarzony.

Dla każdego wierzchołka będziemy przechowywać dwie wartości:
- $SK_n$: maksymalne skojarzenie, w którym korzeń jest skojarzony.
- $SK_n'$: maksymalne skojarzenie nie zawierające korzenia.

Dzięki wartości $SK_n'$ będziemy mogli sprawdzać możliwość połączenia korzenia z wierzchołkami w kolejnych poddrzewach, w zależności od tego, która opcja daje lepszy wynik.

Załóżmy, że mamy drzewo:

![image](https://hackmd.io/_uploads/SJdT_afpJl.png)

Chcemy sprawdzić, do którego poddrzewa najbardziej opłaca nam się podłączyć korzeń. Załóżmy, że dla nas najlepszym wyborem jest podłączenie do wierzchołka 4:

![image](https://hackmd.io/_uploads/BJJkKpGpkx.png)

W takim przypadku musimy policzyć:
- $SK_1 = SK_2 + SK_3 + SK_4' + SK_5 + 1$, ponieważ musimy wybrać opcję, która gwarantuje, że korzeń poddrzewa 4 nie jest połączony z innym wierzchołkiem.
- $SK_1' = SK_2 + SK_3 + SK_4 + SK_5$, czyli suma skojarzeń poddrzew (żadne nie jest podpięte do 1).

## Operatory bitowe

W następnych algorytmach będziemy reprezentować zbiory jako bitowy zapis jakiejś liczby. Oczywiście zera oznaczają, że liczba do zbioru nie należy, a jedynki -- że należy.

Na przykład liczba $13_{10} = 1101_2 = 2^3+2^2+2^0$ utożsamiana jest ze zbiorem $\{0,2,3\}$.

Żeby operować na maskach będziemy korzystać z operatorów bitowych. Zobaczmy je na przykładzie liczb $a = 9_{10}=1001_2$ oraz $b = 12_{10}=1100_2$.

### Logiczna koniunkcja, alternatywa, alternatywa wykluczająca oraz negacja:

**Operator AND (`&`)**

| $a$  | 1 | 1 | 0 | 0 |
|---|---|---|---|---|
| $b$ | 1 | 0 | 0 | 1 |
| $a \& b$  | 1 | 0 | 0 | 0 |

**Operator OR (`|`)**

| $a$  | 1 | 1 | 0 | 0 |
|---|---|---|---|---|
| $b$ | 1 | 0 | 0 | 1 |
| $a\|b$   | 1 | 1 | 0 | 1 |

**Operator XOR (`^`)**

| $a$  | 1 | 1 | 0 | 0 |
|---|---|---|---|---|
| $b$ | 1 | 0 | 0 | 1 |
| $a$ ^ $b$  | 0 | 1 | 0 | 1 |

Ważne, że negacja odwraca też zera wiodące.

$\sim 9_{10}=\ \sim1001_2=111...1110110_2$

**Przesunięcia bitowe:**

- $x << y$ - przesuwa bity $x$ w lewo o $y$ miejsc, równoważne $x \cdot 2^y$,
- $x >> y$ - przesuwa bity $x$ w prawo o $y$ miejsc, równoważne $\lfloor \frac{x}{2^y} \rfloor$.

**Funkcje wbudowane, które mogą się przydać:**

- `__builtin_popcount(x)` - zwraca wartość zapalonych bitów $x$.
- `__builtin_clz(x)` - zwraca ilość zer wiodących liczby $x$, dzięki temu można liczyć $\log$ w czasie stałym.

Używając powyższych operatorów możemy na przykład sprawdzić czy $k$-ty bit liczby $x$ jest zapalony:

```cpp
((1 << k) & x) != 0
```

## Ludzie i maszyny

:::success
Dane jest $N$ pracowników i $N$ maszyn. Dostajemy na wejściu tabelę $N \times N$ wydajności pracownika $L$ na maszynie $M$ i naszym zadaniem jest wybrać ich najefektywniejszy przydział (każdy pracownik musi być przydzielony do dokładnie jednej maszyny, każdy do innej).
:::

| $L \backslash M$ | 0  | 1  | 2  | 3  |
|---|---|---|---|---|
| 0 | 7  | 2  | 3  | 2  |
| 1 | 3  | 2  | 10 | 8  |
| 2 | 5  | 2  | 8  | 3  |
| 3 | 1  | 7  | 1  | 1  |

Na początku spróbujmy rozwiązać ten problem korzystając z backtrackingu. Będziemy mieć tablicę o rozmiarze $N$, gdzie przetestujemy wszystkie kombinacje. Pseudokod wyglądałby mniej więcej tak:

```plaintext
backtrack(nr)
    if (nr == n) przypadek bazowy
    dla każdego wolnego stanowiska x:
        wstaw pracownika nr na stanowisku x
        backtrack(nr+1)
        cofnij wstawienie
```

Łatwo jednak zauważyć, że ten algorytm potrzebuje dwóch rzeczy:

- musi znać zajęte stanowiska, ale nie musi wiedzieć dokładnie przez kogo,
- suma wydajności przydzielonych już pracowników.

Przykładowo dla maski:

| $m$   | 1 | 0 | 0 | 1 |
| --- | - | - | - | - |
| $L$   | 7 |   |   | 2 |
| $L'$  | 4 |   |   | 6 |

Nie jest tu istotne, czy pod maską znajduje się zestaw $L$ czy $L'$, ponieważ interesuje nas tylko najwyższa suma. Załóżmy więc, że maska zawsze ma najlepsze rozstawienie.

Dzięki tej obserwacji możemy zmienić nieco podejście. Zamiast pamiętać, kto był na którym stanowisku, będziemy uzupełniać odpowiednią maskę bitową.

- $\text{DP[MASKA]}$ - największy zysk dla pierwszych $k$ osób (gdzie $k$ oznacza liczbę zapalonych bitów w masce),
- $\text{MASKA} \in [0, 2^n-1]$.

```plaintext
dla każdej osoby nr:
    dla każdej maski m, która ma zapalonych nr bitów:
        dla każdej wolnej pozycji i (w masce m):
            zaktualizuj odpowiednie
```

Złożoność tego rozwiązania to $O(n^2 \cdot 2^n)$, ale da się to oszacować lepiej.

## Liczba ścieżek Hamiltona

:::success
Zadanie. Mamy dany graf skierowany o wierzchołkach $[1, n]$. Chcemy policzyć takie ścieżki, które zaczynają się w wierzchołku $1$, kończą w wierzchołku $n$ i przechodzą dokładnie raz przez każdy wierzchołek.
:::

Zastanówmy się najpierw, jak napisać rozwiązanie wykorzystujące metodę nawrotów (backtracking).

```plaintext
backtrack(u)
    if(u == n) przypadek bazowy
    dla każdego nieodwiedzonego sąsiada u, v
        backtrack(v)
        ustaw v jako nieodwiedzony
```

Zamiast tego możemy operując na maskach bitowych zaznaczać dokładnie to samo.

Niech $\text{DP[MASKA][v]}$ oznacza liczbę ścieżek Hamiltona przechodzących przez wierzchołki z maski, od wierzchołka $1$ do wierzchołka $v$, gdzie $v$ jest ostatnio odwiedzonym na tej ścieżce.

Pseudokod dla tego pomysłu wyglądałby mniej więcej tak:


```plaintext
dla każdej maski m
    jeśli wierzchołek v był odwiedzony
        jeśli wierzchołek u był odwiedzony
            dodaj do DP[m][v]
```
# Wykład 6: Drzewa przedziałowe

## Sumy prefiksowe

:::success
Zadanie. Mamy sumy prefiksowe i dwie operacje:

- **policz(k)**: $a_1 + a_2 + ... + a_k$
- **zamień(k, x)**: $a_k = x$
:::

Weźmy ciąg: $1, 7, 2, 4, 3, 9, 8, 8$. Ustawimy jego elementy na liściach drzewa binarnego zrównoważonego, a każdy następny węzeł zbudujemy z sumy jego dzieci. Wyglądałoby ono w ten sposób. Możemy na nim przykładowo obliczyć **policz(7)**. Widać, że nasza struktura pozwala wybrać gotową sumę czterech elementów, dwóch i jednego.

![image](https://hackmd.io/_uploads/rkwHbVipyx.png)

Które elementy sumujemy w tym procesie? Spójrzmy na ścieżkę od elementu 7 do korzenia. Mamy dwa przypadki:

- idziemy w górę z **lewego dziecka** – suma jest już policzona,
- idziemy w górę z **prawego dziecka** – dodajemy lewe dziecko do wyniku.

![image](https://hackmd.io/_uploads/H1Sv-Ei6kl.png)

Gdybyśmy chcieli obsłużyć poprawnie operację **zamień(k, x)**, wystarczy podmienić wartość liścia i zaktualizować sumy na ścieżce do korzenia.

### Reprezentacja drzewa w pamięci

Zastanówmy się, jak można przechowywać takie drzewo w pamięci. Ponumerujmy wierzchołki od korzenia w dół. Dla naszego przykładu wygląda to tak:

![image](https://hackmd.io/_uploads/SJk9WEiT1x.png)

Można zauważyć, że każdy wierzchołek zachowuje własność:

![image](https://hackmd.io/_uploads/HyljZVs6yl.png)

Widać też, że każde **lewe dziecko** ma numer parzysty, a **prawe dziecko** – nieparzysty. Dzięki temu łatwo zaimplementować wcześniejszą obserwację dla obliczania sum prefiksowych.

To pozwala nam umieścić elementy w tablicy. Dla naszego przykładu wyglądałoby to w ten sposób:

| i       | 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  | 9  | 10 | 11 | 12 | 13 | 14 | 15 |
|---------|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| T[i]  | 42 | 14 | 28 | 8  | 6  | 12 | 16 | 1  | 7  | 2  | 4  | 3  | 9  | 8  | 8  |

Ten sposób działa poprawnie, gdy liczba elementów ciągu jest potęgą dwójki. Jak sobie poradzić, gdy tak nie jest? Wystarczy wybrać takie $k$, że $S = 2^k \geq n$ i wypełnić część tablicy **elementami neutralnymi** dla danej operacji (w przypadku dodawania – zerami). Podczas wczytywania wartości wystarczy wczytywać dane od indeksu $S$ do końca, ponieważ tam jest miejsce na liście naszego drzewa.

## Inwersje w ciągu

:::info
**Inwersją** w ciągu nazwiemy parę dwóch elementów, w której element znajdujący się po lewej stronie jest większy od elementu po prawej.
:::

Rozważmy ciąg: **5, 3, 7, 6, 4**. Policzmy inwersje sprawdzając dla każdego elementu ile jest elementów większych od niego, które są po jego lewej stronie.

W tym przykładzie mamy:

- **5** – pierwsza liczba, **0 inwersji**
- **3** – $5 > 3$, **1 inwersja**
- **7** – największa dotychczas liczba, nadal **1 inwersja**
- **6** – $7 > 6$, **2 inwersje**
- **4** – $5 > 4$, $7 > 4$, $6 > 4$, **5 inwersji**

### Tablica wystąpień

Skorzystamy z tablicy $T[1 \dots {max}]$, która będzie zliczała wystąpienia każdej liczby w ciągu. Będziemy po kolei dodawać kolejne elementy i aktualizować wynik.

| Iteracja  | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | Inwersje |
|-----------|---|---|---|---|---|---|---|---|---|-------|
| Początek  | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0     |
| Dodaj 5   | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 0     |
| Dodaj 3   | 0 | 0 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 1     |
| Dodaj 7   | 0 | 0 | 1 | 0 | 1 | 0 | 1 | 0 | 0 | 1     |
| Dodaj 6   | 0 | 0 | 1 | 0 | 1 | 1 | 1 | 0 | 0 | 2     |
| Dodaj 4   | 0 | 0 | 1 | 1 | 1 | 1 | 1 | 0 | 0 | 5     |

Teraz widać, że za każdym razem, gdy dodajemy wartość do tablicy wystąpień, wynik zwiększa się o **sumę sufiksową** od dodanego elementu.

### Efektywna implementacja na drzewie przedziałowym

Aby obliczać liczbę inwersji efektywnie, możemy skorzystać z **drzewa przedziałowego**:

1. Dodajemy do wyniku sumę wartości na przedziale $[v, max]$, gdzie $v$ to właśnie dodany element.
2. Zwiększamy o jeden wartość w liściu odpowiadającym danej liczbie.
3. Aktualizujemy drzewo sum sufiksowych.

### Liczenie sumy sufiksowej

Sumę sufiksową możemy obliczyć na dwa sposoby:

1. **Sumy prefiksowe na odwrót** – korzystamy odwrotnie z wcześniejszej obserwacji o parzystości.
2. **Odejmowanie sumy prefiksowej** – bierzemy sumę całego drzewa i odejmujemy sumę prefiksową do $v-1$.

Dzięki tym metodom możemy w czasie $O(\log n)$ dla każdej operacji skutecznie policzyć liczbę inwersji w danym ciągu.

## Funkcja rekurencyjna

Dla drzew możemy zaimplementować ogólną funkcję rekurencyjną, w której w zależności od problemu wystarczy zmienić odpowiednio zwracane dane.
W poniższej funkcji `nr` oznacza numer wierzchołka, któr aktualnie sprawdzamy, `z` to zakres za który ten wierzechołek odpowiada, a `p` to zliczany przedział.

```plaintext
policz(nr, z, p)
    if(z==p) zwróć wynik
    s = (z.pocz + z.kon) / 2
    if p należy do [z.pocz, s]
        policz(2 * nr, [z.pocz, s], p)
    else if p należy do [s+1, z.kon]
        policz(2 * nr + 1, [s+1, z.kon], p)
    else 
        rozcinamy przedział:
            wywołaj się z [p.pocz, s] na lewej części
            wywołaj się z [s+1, p.kon] na prawej części
```

Ogólnym zamysłem jest rozdzielenie podproblemów na przedziały, które potrafią je rozwiązać. Mamy w funkcji trzy przypadki:

* Przedział w całości zawiera się po lewej stronie – wywołujemy się po lewej stronie.
* Przedział w całości zawiera się po prawej stronie – wywołujemy się po prawej stronie.
* Przedział leży gdzieś po środku – dzielimy przedział na lewą i prawą stronę, po czym wywołujemy się naobu częściach w analogiczny sposób.

## Modyfikacja na przedziale

:::success
Zadanie
Mamy dany ciąg $a_1, a_2, ..., a_N$ i dwie operacje:
- zwiększ wartości na przedziale $[x,y]$ o $v$,
- podaj aktualną wartość $a_k$.
:::

Możemy łatwo zmodyfikować wcześniej napisaną funkcję rekurencyjną, by rozdzielała odpowiednio modyfikacje.  Będziemy je sumować dopiero wtedy, kiedy musimy je wypisać.  

Przykładowo mogłoby wyglądać to tak:

![image](https://hackmd.io/_uploads/SkQyDd26kg.png)

Żeby wydobyć wartość pojedynczego elementu, wystarczy przejść jego ścieżką do korzenia i zsumować wszystkie napotkane po drodze modyfikacje.

## Maksymalne prefiksy

:::success
Zadanie. Mamy dany ciąg $a_1, a_2, ..., a_N$ i dostępne dwie operacje:
- zmień wartość $a_k$,
- policz maksymalną sumę prefiksową na przedziale $[x,y]$.
:::

Weźmy taki ciąg:
| $i$   | 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  |
|---------|----|----|----|----|----|----|----|----|
| $T[i]$| 1  | 3  | -5 | -4 | 3  | 2  | -3 | 7  |

Przykłady zapytań:
- dla $[4,7]$ maksymalnym prefiksem jest **1**,
- dla $[4,8]$, **5**,
- dla $[2,7]$, **3**.

Załóżmy, że mamy dwa przedziały, **lewy** i **prawy**, gdzie znamy już maksymalne prefiksy. Chcemy ustalić teraz największy prefiks dla przedziału składającego się z lewego i prawego. Musimy rozważyć dwa przypadki:

- maksymalny prefiks kończy się w lewej części – wtedy bierzemy prefiks z lewej części i przekazujemy go dalej,
- maksymalny prefiks kończy się w prawej części – wtedy musimy zsumować wszystkie elementy z lewej części przedziału i dodać maksymalny prefiks z prawej części.

W drzewie możemy realizować to w ten sposób:

![image](https://hackmd.io/_uploads/rkssdO3pyl.png)

Można to łatwo zaimplementować, odpowiednio modyfikując wcześniejszą funkcję rekurencyjną, aby zwracała potrzebną nam parę wartości.

## Maksymalne sumy


:::success
Zadanie. Dany jest ciąg $a_1, a_2, ..., a_k$ i operacja zmiany wartości $k$-tego elementu ciągu. Naszym zadaniem jest znaleźć największą sumę spójnego podciągu po każdej zmianie wartości.
:::

Przykładowo, dla poniższego ciągu największa suma wynosi **9** na przedziale $[5,8]$:

| $i$   | 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  |
|---------|----|----|----|----|----|----|----|----|
| $T[i]$| 1  | 3  | -5 | -4 | 3  | 2  | -3 | 7  |

Wyobraźmy sobie, że budujemy **drzewo przedziałowe**, w którym każdy węzeł przechowuje informacje o pewnym fragmencie tablicy. 

Aby to osiągnąć, dla każdego przedziału zapamiętujemy **cztery kluczowe wartości**:
- **suma** wszystkich elementów w przedziale,
- **najlepszy prefiks** – największa suma początkowego fragmentu przedziału,
- **najlepszy sufiks** – największa suma końcowego fragmentu przedziału,
- **największa suma** spójnego podciągu wewnątrz przedziału.

### Łączenie przedziałów  
Każdy przedział składa się z dwóch mniejszych (lewego i prawego). Aby obliczyć wartości dla większego przedziału na podstawie jego dwóch podprzedziałów, musimy odpowiednio zaktualizować dane:

- **suma** – po prostu sumujemy sumy obu przedziałów,
- **prefiks** – tak, jak w poprzednim problemie,
- **sufiks** – symetrycznie do prefiksu,
- **maksymalna suma** – może pochodzić z maksymalnego spójnego podciągu lewego przedziału, prawego przedziału **albo** podciągu, który łączy sufiks lewego przedziału z prefiksem prawego.

Ponownie możemy wykorzystać naszą funkcję rekurencyjną, ale tym razem musimy przechodzić przez nią, manipulując czterema wartościami jednocześnie.

# Wykład 7: Drzewa przedziałowe cd.

## Pierwsza mniejsza lub równa wartość
:::success
Zadanie. Dany jest ciąg $a_1,...,a_n$ oraz zapytania:
* $\text{znajdz(x)}$: znajdź indeks pierwszej wartości mniejszej lub równej $x$
* $\text{zamien(i, x)}$: zamień $a_i$ na $x$
:::

Dla przykładowego ciągu i zapytań:


| $i$  | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|------|---|---|---|---|---|---|---|---|
|$T[i]$| 7 | 9 | 5 | 3 | 6 | 2 | 7 | 7 |

* $\text{znajdz(8)}$ – wynik: indeks $1$.
* $\text{znajdz(4)}$ – wynik: indeks $4$.
* $\text{zamien(4, 13)}$ – wykonujemy zamianę czwartej wartości na **13**
* $\text{znajdz(4)}$ – wynik: indeks $6$, ponieważ po zamianie wartość na pozycji $4$ uległa zmianie, co przesunęło wynik.

Do rozwiązania tego zadania wykorzystamy drzewa przedziałowe. W węzłach drzewa umieścimy minima fragmentów ciągu. Dzięki temu będziemy wiedzieć, w której części drzewa znajduje się interesująca nas wartość.

![image](https://hackmd.io/_uploads/B1uz_9rAJl.png)


Można zauważyć, że poruszamy się w drzewie w następujący sposób:
* sprawdzamy w pierwszej kolejności lewego syna, jeśli ma mniejszą lub równą wartość od zadanej, to przechodzimy do niego,
* w przeciwnym przypadku sprawdzamy czy prawy syn ma mniejszą lub równą wartość.

-----
## Usuwanie z listy

:::success
Zadanie. Dana jest **lista** $a_1, a_2, \dots, a_n$ oraz zapytanie:
- $\text{usun(i)}$: wypisz oraz usuń $i$-ty element z listy.
:::

### Przykład

Rozważmy listę:

$$
7 \rightarrow 3 \rightarrow 2 \rightarrow 8 \rightarrow 5 \rightarrow 1
$$

- $\text{usun(3)}$: usuwa trzeci element, czyli $2$:

  $$
  7 \rightarrow 3 \rightarrow 8 \rightarrow 5 \rightarrow 1
  $$

- $\text{usun(3)}$: teraz trzeci element to $8$:

  $$
  7 \rightarrow 3 \rightarrow 5 \rightarrow 1
  $$

Rozwiążemy to zadanie, używając drzewa przedziałowego, w którym każdy węzeł przechowuje informację o liczbie elementów w odpowiadającym mu przedziale (czyli sumę „obecności” elementów). Dzięki temu możemy szybko znaleźć $k$-ty element.

![image](https://hackmd.io/_uploads/ByAf95BCyg.png)

Będziemy szukać szóstego elementu patrząc najpierw na lewą część, potem na prawą. Zaczynamy od korzenia.
- Lewe poddrzewo nie ma szóstego elementu, bo jego suma elementów to tylko cztery. 
- Szósty element w drzewie to drugi element w prawym poddrzewie (odejmujemy to, czego nie ma w lewej części).

W ten sposób schodzimy wgłąb, aż do liścia. Tutaj jest nasze rozwiązanie.

### Ogólna metoda

Szukanie $k$-tego elementu wygląda w ten sposób:
- Zaczynając od korzenia, porównujemy $k$ z sumą wartości lewego dziecka.
- Jeśli suma lewego dziecka jest większa lub równa $k$, to szukamy dalej w lewym poddrzewie.	
- Jeśli suma lewego dziecka jest mniejsza od $k$, odejmujemy tę wartość od $k$ i szukamy w prawym poddrzewie.

## Modyfikacja i maksimum

:::success
Zadanie. Dany jest ciąg $a_1, ..., a_n$ oraz operacje:
- $\text{dodaj(p, k, x)}$: dodaj $x$ do przedziału $[p, k]$
- $\text{max(p, k)}$: znajdź maksimum na przedziale $[p, k]$
:::

Można rozwiązać to zadanie na dwa sposoby - korzystając z leniwej propagacji całkowicie lub z jej części. Przedstawimy tutaj drugie podejście.

Omawialiśmy już maksima i modyfikacje na przedziałach. Jak się okazuje, połączenie tych dwóch dobrze znanych technik niemal całkowicie rozwiązuje ten problem – wystarczy odpowiednio zmodyfikować sposób aktualizacji i zapytań.

Każdy węzeł w naszym drzewie będzie zawierał dwie informacje: maksimum w poddrzewie oraz modyfikację dla danego przedziału.

Żeby poprawnie obsłużyć zapytania wykorzystamy dwie funkcje rekurencyjne – jedną do dokładania przedziałów (`dodaj(p, k, x)`), którą omówiliśmy już na poprzednim wykładzie, a drugą do znajdowania maksimum na przedziale (`max(p, k)`).

Aby poprawnie odpowiadać na pytania o maksimum, musimy wziąć pod uwagę modyfikacje z wyższych poziomów drzewa. Sumaryczny efekt modyfikacji możemy policzyć poprzez dodanie do maksimum sumy modyfikacji ze ścieżki z korzenia (tak jak w algorytmie z modyfikacja na przedziale, ale jest to dodatkowy logarytm), lub – jak poniżej – przekazywać jako parametr rekurencji zapytania o maximum (policzymy to po drodze).

```plaintext
policz_max(nr, z, p, ścieżka)
    if(z==p) zwróć wynik zwiększony o ścieżkę
    s = (z.pocz + z.kon) / 2
    if p należy do [z.pocz, s]
        policz(2 * nr, [z.pocz, s], p, ścieżka + modyfikacja nr)
    else if p należy do [s+1, z.kon]
        policz(2 * nr + 1, [s+1, z.kon], p, ścieżka + modyfikacja nr)
    else 
        rozcinamy przedział:
            wywołaj się z [p.pocz, s] na lewej części
            wywołaj się z [s+1, p.kon] na prawej części
            aktualizuj max dla lewego i prawego wyniku rekurencji
```

## Ustawianie wartości i maksimum

:::success
Zadanie. Dany jest ciąg $a_1, ..., a_n$ oraz operacje:
- $\text{ustaw(p, k, x)}$: ustaw wartość $x$ na przedziale $[p, k]$,
- $\text{max(p, k)}$: znajdź maksimum na przedziale $[p, k]$.
:::

Wcześniejsze podejście, którego używaliśmy do modyfikacji przedziałów (np. dodawania wartości), nie zadziała w przypadku **ustawiania**, ponieważ tutaj znaczenie ma kolejność,w  jakiej były wykonywane modyfikacje. Zobaczmy na taki przykład:

![image](https://hackmd.io/_uploads/S13bGsrC1e.png)

Rozważmy dwa możliwe porządki zapytań:

#### Przypadek 1:

- $\text{ustaw(1, 4, 7)}$
- $\text{ustaw(3, 4, 5)}$
- $\text{ustaw(4, 4, 4)}$

Wynikowy ciąg:

| $i$       | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|-----------|---|---|---|---|---|---|---|---|
| $T[i]$    | 7 | 7 | 5 | 4 |   |   |   |   |

#### Przypadek 2:

- $\text{ustaw(3, 4, 5)}$
- $\text{ustaw(1, 4, 7)}$
- $\text{ustaw(4, 4, 4)}$

Wynikowy ciąg:

| $i$       | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|-----------|---|---|---|---|---|---|---|---|
| $T[i]$    | 7 | 7 | 7 | 4 |   |   |   |   |

Te dwa przypadki pokazują, że nasza reprezentacja na drzewie jest niejednoznaczna - jej wynikiem są dwa różne ciągi. 

Poprawnym podejściem jest zastosowanie **leniwej propagacji (lazy propagation)**.

---

### Jak działa leniwa propagacja?

Wierzchołki drzewa nie będą od razu przekazywać wartości swoim dzieciom. Zamiast tego będą **odkładać przekazanie zmian** aż do momentu, gdy będzie to potrzebne (np. w czasie zapytania lub dalszej modyfikacji).

Każdy węzeł będzie oznaczony jako **leniwy**, jeśli musi jeszcze przekazać wartość swoim dzieciom.

---

### Schemat działania algorytmu

1. Schodząc w dół drzewa, jeśli węzeł ma nieprzekazane ustawienie (jest leniwy):
   - przekaż wartość dzieciom,
   - ustawiamy tę wartość w synach (zarówno `max`, jak i `lazy`).
2. Oznaczamy bieżący węzeł jako nie-leniwy (czyli wykonano przekazanie).
3. Przechodzimy dalej zgodnie z rekurencją — aktualizuj wartości tylko wtedy, gdy potrzeba.

# Wykład 8: Operacje na drzewach

## Binary Lifting

:::success
Zadanie. Dane jest drzewo ukorzenione oraz zapytania:

- $\text{przodek(v,k)}$: przodek $k$-tego stopnia wierzchołka $v$
:::

Oczywiście, możemy za każdym razem $k$-razy przechodzić po rodzicach. Jednak byłoby to rozwiązanie liniowe, my będziemy szukać czegoś znacznie szybszego.

Wykorzystamy metodę preprocessingu taką, jak w Sparse Table. Dla każdego wierzchołka przetrzymamy informacje o przodkach pierwszym, drugim, czwartym, itd.

Przechowamy to w tablicy $\text{up} [N][\log N]$, gdzie

$$\text{up[u][k]} = \text{przodek(u,}2^k\text{)}$$

Możemy budować tę tablicę z mniejszych wyników:

- $\text{up[u][0]}$ – to po prostu ojciec $u$ 
- $\text{up[u][1]}$ = $\text{up[up[u][0]][0]}$  
- $\text{up[u][2]}$ = $\text{up[up[u][1]][1]}$    
- $\text{up[u][k]}$ = $\text{up[up[u][k-1]][k-1]}$

Teraz, aby znaleźć $k$-tego przodka wystarczy spojrzeć na zapis binarny liczby $k$ oraz wykonać skoki binarne o odpowiednie potęgi dwójki.

## Najniższy wspólny przodek

:::success
Zadanie. Dane jest drzewo ukorzenione i zapytanie:

- $\text{LCA(u, v)}$: najniższy wspólny przodek $u$ i $v$
:::

Zobaczmy na przykład.  

![image](https://hackmd.io/_uploads/By3rK-Nkex.png)

Wykorzystamy do tego **Binary Lifting**. Będziemy z wierzchołka $v$ szukać najwyższego przodka, który nie jest wspólny dla $u$ i $v$.

![image](https://hackmd.io/_uploads/HJd4Yb41xx.png)


W jaki sposób możemy sprawdzać, czy dany wierzchołek jest wspólny dla $u$ i $v$? Przejdziemy po drzewku DFS-em i w każdym wierzchołku zapamiętajmy *czas wejścia* oraz *czas wyjścia* przy pomocy licznika, który będzie się zwiększał w momencie wejścia i wyjścia do wierzchołka.

![image](https://hackmd.io/_uploads/H1qdcWVklg.png)

Teraz, dla przykładu, możemy łatwo sprawdzić, czy 7 jest wnukiem 9 porównując powyższe czasy, bo widać, że:

- $14 \geq 12$,
- $15 \leq 25$,

zatem wierzchołek 7 znajduje się w poddrzewie wierzchołka 9.

Przy szukaniu LCA będą nas interesowały te wierzchołki, które **nie spełniają** tego warunku.
Ojciec znalezionego w ten sposób wierzchołka jest LCA wierzchołków z zapytania.

To zadanie można rozwiązać jeszcze inną metodą. Potrafimy ustalić głębokości wierzchołków poprzez DFS. Dzięki temu możemy najpierw **wyrównać wierzchołki**, aby były na tej samej głębokości, wykonując odpowiednie skoki wierzchołkiem, który jest niżej. Dalej, wykonując skoki tej samej długości z obu wierzchołków, wystarczy znaleźć -- tak jak wcześniej – największych przodków, którzy **nie są wspólni** dla $u$ i $v$, a dokładniej w tym przypadku -- tych, którzy **są różni**.

Na przykładzie:  

![image](https://hackmd.io/_uploads/H1w3sWNklx.png)

## Odległość wierzchołków

:::success
Zadanie. Dane jest drzewo (nieukorzenione) oraz zapytanie:

- $\text{odleglosc(u, v)}$: odległość wierzchołka $u$ od $v$
:::

To zadanie jest bardzo łatwe, kiedy mamy już algorytm do znajdowania LCA. Na początku wybierzmy dowolny korzeń w drzewie. Następnie zauważmy, że ścieżka między wierzchołkami $u$ i $v$ musi przechodzić przez ich najniższego wspólnego przodka. Dzięki temu możemy podzielić zapytanie na $\text{odleglosc(u, LCA(u, v))}$ oraz $\text{odleglosc(v, LCA(u, v))}$, a to są już odległości na ścieżce, które można łatwo policzyć **odejmując głębokości**.

Dla przykładu:  

![image](https://hackmd.io/_uploads/HylH0-Ekgg.png)

Zatem naszym wynikiem jest

$$\text{glebokosc(u)} + \text{glebokosc(v)} - 2 \cdot \text{glebokosc(LCA(u,v))}.$$

Możemy rozwiązać to zadanie także bez użycia Binary Liftingu. Na początku uzupełniamy głębokości wierzchołków oraz wykonujemy **Euler Tour**. W wyniku otrzymujemy coś takiego:

![image](https://hackmd.io/_uploads/By4FGR81xe.png)

Trzymamy te dane w dwóch tablicach:

| Wierzchołki | 1 | 2 | 4 | 2 | 5 | 8 | 5 | 2 | 6 | 2 | 1 | 9 | 3 | ⋯ |
|-------------|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Głębokości  | 1 | 2 | 3 | 2 | 3 | 4 | 3 | 2 | 3 | 2 | 1 | 2 | 3 | ⋯ |

Możemy ustalić głębokość $\text{LCA}(u, v)$ patrząc na fragment przejścia Eulera pomiędzy **pierwszymi wystąpieniami** $u$ i $v$ (a jeśli się dokładniej zastanowić, mogą to też być dowolne wystąpienia tych wierzchołków). Ich najmniejszym wspólnym przodkiem będzie wierzchołek o **najmniejszej głębokości** na tym przedziale.

Przykładowo:

- $\text{glebokosc(LCA(4,8))} = 2$
- $\text{glebokosc(LCA(5,9))} = 1$
- $\text{glebokosc(LCA(4,5))} = 2$

Aby znaleźć najmniejszą głębokość na danym przedziale, możemy skorzystać z drzewa przedziałowego.

## Zliczanie ścieżek

:::success
Zadanie. Dane jest drzewo ukorzenione oraz zbiór ścieżek. Dla każdego wierzchołka określ ilość ścieżek przechodzących przez niego.
:::

Dla przykładu mamy takie ścieżki:

- $(13, 6)$
- $(10, 12)$
- $(7, 9)$

![image](https://hackmd.io/_uploads/SkRbJM4yxx.png)

Dla tego przykładu liczba ścieżek dla wierzchołków wyglądałaby tak:

| $i$            | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 |
|----------------|---|---|---|---|---|---|---|---|---|----|----|----|----|----|
| $sciezki[i]$   | 0 | 2 | 2 | 2 | 3 | 2 | 1 | 0 | 2 |  1 |  0 |  2 |  1 |  0 |

Aby policzyć coś takiego, będziemy nakładać **modyfikacje od wierzchołka w górę**:

- $+1$ od $u$
- $+1$ od $v$
- $-1$ od $\text{LCA(u, v)}$
- $-1$ od ojca $\text{LCA(u, v)}$

![image](https://hackmd.io/_uploads/H1EYkGEJex.png)

Aby odpowiednio rozprowadzić te modyfikacje, wystarczy przejść **DFS-em po całości** i w każdym wierzchołku policzyć sumę modyfikacji w jego poddrzewie.

## Sumy w poddrzewach

:::success
Zadanie. Dane jest drzewo ukorzenione, wartości wierzchołków oraz zapytanie:
- $\text{suma(v)}$: suma wartości wierzchołków z poddrzewa $v$
:::

Weźmy dla przykładu takie drzewko:

![image](https://hackmd.io/_uploads/H1cO7CLJge.png)

Przejdziemy po drzewku ścieżką Eulera i zapiszemy wartości w taki sposób:

- wejście do $v$ – wartość wierzchołka $v$
- wyjście z $v$ – $0$

| Wierzchołki | 1 | 2 | 4 | 2 | 5 | 8 | 5 | 2 | 6 | 2 | 1 | 9 | 3 | ⋯ |
|-------------|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Wartości    | 5 | 7 | 3 | 0 | 1 | 5 | 0 | 0 | 4 | 0 | 0 | 2 | 4 | ⋯ |

Sumę poddrzewa można wyznaczyć, sumując przedział od **pierwszego** do **ostatniego wystąpienia** wierzchołka $v$ w ścieżce Eulera. Do obliczeń można użyć **segment tree**.

Przykłady:

- $\text{suma(2)} = 7 + 3 + 0 + 1 + 5 + 0 + 0 + 4 + 0$
- $\text{suma(4)} = 3$

## Sumy na ścieżkach

:::success
Zadanie. Dane jest drzewo ukorzenione oraz zapytanie:

- $\text{sciezka(v)}$: suma wartości ścieżki od korzenia do $v$
:::

Dla przykładu drzewko z $\text{sciezka(6)}$: 

![image](https://hackmd.io/_uploads/SyUxVR8Jxe.png)

Przejdziemy po drzewku ścieżką Eulera i zapiszemy wartości w taki sposób:

- wejście do $v$ – wartość wierzchołka $v$
- wyjście z $v$ – przeciwna wartość wierzchołka $v$

| Wierzchołki | 1 | 2 | 4 | 4 | 5 | 8 | 8 | 5 | 6 | 6 | 2 | 9 | 9 | ⋯ |
|-------------|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Wartości    | 5 | 7 | 3 | -3 | 1 | 5 | -5 | -1 | 4 | -4 | -7 | 2 | -2 | ⋯ |

Aby wydobyć sumę wartości ścieżki od korzenia do wierzchołka $v$, wystarczy wziąć sumę prefiksową od korzenia do **wejścia** do $v$. Można to szybko obliczyć za pomocą drzewa przedziałowego.

- $\text{ścieżka(6)} = 5 + 7 + 3$
- $\text{ścieżka(8)} = 5 + 7 + 3 - 3 + 1 + 5$

# Wykład 9: Grafy z wagami

## Algorytm Dijkstry

:::success
Zadanie. Dany jest graf z wagami (nieujemnymi). Znajdź najkrótszą ścieżkę z $u$ do $v$.
:::

Dla przykładu:

![image](https://hackmd.io/_uploads/Hyck2s1gel.png)

Najkrótsza ścieżka w tym grafie między $u$ i $v$ to $3 + 2 + 4$.

Algorytm Dijkstry wyznacza najkrótszą ścieżkę z $u$ do **każdego** wierzchołka. 

Mamy taki graf:

![image](https://hackmd.io/_uploads/B1gX3jJeel.png)

Będziemy uzupełniać ten wierzchołek, który jest **najbliżej** $u$.

![image](https://hackmd.io/_uploads/S1AX3iyxge.png)

Ustawiamy wierzchołek startowy jako $0$ i próbujemy przejść dalej. Uzupełniamy jako pierwszy $0 + 1$, bo $0 + 3$ jest dalej.

![image](https://hackmd.io/_uploads/S14S2oyxll.png)

Teraz widać, że wartość drugiego wierzchołka byłaby niepoprawna, gdybyśmy od razu ją uzupełnili. Okazało się, że jest lepsza ścieżka i zamiast $0 + 3$ mamy w tym miejscu $1 + 1$. 

![image](https://hackmd.io/_uploads/H1oS2s1glx.png)

Na samym końcu wybierzemy do uzupełnienia najpierw $2 + 5$, ponieważ dla ostatniego wierzchołka być może znaleźlibyśmy w innym przypadku lepszą ścieżkę. 

![image](https://hackmd.io/_uploads/H12P2ikell.png)

Okazuje się, że $4 + 4$ było najlepszym rozwiązaniem, ale uzupełnimy je dopiero na samym końcu.

Odtworzyć taką ścieżkę można na dwa sposoby:

- zapamiętując wskaźnik na poprzednio odwiedzony wierzchołek (dodatkowa tablica),

  ![image](https://hackmd.io/_uploads/SkCdnsJexx.png)
	
- dobierając poprzedni wierzchołek patrząc na sumę krawędzi i wagi poprzedniego wierzchołka.

## Implementacja Algorytmu Dijkstry

Weźmy taki graf:

![image](https://hackmd.io/_uploads/Sylk6okxgl.png)

Będziemy przypisywać po kolei wartości i wybierać nowe minima.

Zaczniemy od przypisania $u = 0$. Generuje nam ono dwie możliwości: 
- $(a,1)$
- $(b,3)$

Wybieramy minimum, więc przypisujemy $a = 1$. Aktualizujemy możliwości:
- $(b,3)$ (z poprzedniej)
- $(b,2)$
- $(c,6)$

Najlepszą opcją jest $(b,2)$, więc przypisujemy $b = 2$. Aktualizujemy możliwości:
- ~~(b,3)~~ (wyrzucamy, bo znalazła się lepsza)
- $(c,6)$
- $(c,4)$
- $(d,7)$

Wybieramy $c = 4$:
- ~~(c,6)~~
- $(d,7)$
- $(v,8)$

Następnie $d = 7$:
- $(v,8)$
- $(v,10)$

**Odpowiedź:** $v = 8$.

Zauważmy, że za każdym razem interesuje nas zapytanie, które ma najmniejszy koszt. Dzięki temu możemy wybrać strukturę, która podsuwa nam od razu najmniejszy element, np.:
- kolejka priorytetowa,
- kopiec,
- $\texttt{set}$ w C++.

Warto zauważyć, że powyższy algorytm jest poprawny, gdyż nasz graf nie zawiera krawędzi o ujemnych wagach. Gdyby tak nie było, nie mielibyśmy pewności, czy przypisane przez nas wagi nie zostaną później zmniejszone, co wymagałoby ponownego rozpatrzenia tych wierzchołków.

## Liczba ścieżek

:::success
Zadanie. Dany jest graf z wagami (nieujemnymi). Podaj ilość najkrótszych ścieżek z $u$ do $v$. 
:::

Dla przykładu:

![image](https://hackmd.io/_uploads/HyjlAiklxx.png)

Są tutaj trzy ścieżki między $u$ i $v$, których długości to $3$.

Żeby znaleźć liczbę najkrótszych ścieżek z $u$ do $v$, wystarczy trzymać w wierzchołkach dodatkową informację o tym, ile takich najkrótszych ścieżek do niego prowadzi. 

![image](https://hackmd.io/_uploads/H1vbRskgll.png)

## Najkrótsza ścieżka z kuponem

:::success
Zadanie. Dany jest graf z wagami (nieujemnymi) oraz kupon, który pozwala nam zmniejszyć wagę jednej krawędzi dwukrotnie (dzieląc całkowicie). 
Znajdź najkrótszą ścieżkę z $u$ do $v$.
:::

Dla przykładu:

![image](https://hackmd.io/_uploads/H1uL0jkxxg.png)

Najkrótsza ścieżka po użyciu kuponu na krawędzi o wadze $9$ to $1+4$.

Podzielimy teraz graf na dwa światy. Jeden, w którym jeszcze nie użyliśmy kuponu, drugi — po użyciu.

![image](https://hackmd.io/_uploads/Syha0s1exe.png)

Aby rozwiązać to zadanie, wystarczy te światy połączyć. Zauważmy, że warstwa z nieużytym kuponem prowadzi do tej z użytym po zastosowaniu go na jakiejś krawędzi. Zatem możemy połączyć te światy w poniżej przedstawiony sposób (dla czytelności na rysunku dodane zostały tylko istotne dla nas krawędzie).

![image](https://hackmd.io/_uploads/SkDly3Jxll.png)

Teraz z dwóch grafów robi nam się jeden duży graf. Jedyne co musimy zrobić to przejść algorytmem Dijkstry po nim i wyznaczyć najkrótszą ścieżkę.

## Algorytm Bellmana-Forda

:::success
Zadanie. Dany jest graf skierowany z wagami, które mogą być ujemne. Znajdź najkrótszą ścieżkę z $u$ do $v$.
:::

Teraz nie mamy gwarancji, że nasze wagi są nieujemne, więc algorytm Dijkstry może nie działać poprawnie. Na przykład:

![image](https://hackmd.io/_uploads/BknYynylge.png)

Algorytm zwróci nam, że najkrótsza ścieżka to $2$, ponieważ założyliśmy, że wybieramy zawsze mniejszą. A skoro wierzchołek $v$ został uzupełniony z kosztem $2$, to powinien być optymalny (założenie). 

Teraz mając ujemne krawędzie, widać, że nie możemy od razu założyć, że wierzchołki są optymalne.

Warto też zauważyć, że jeśli graf posiada **ujemny cykl**, to nie ma najkrótszej ścieżki. Można kręcić się nieskończenie wiele razy, co skraca naszą ścieżkę.

![image](https://hackmd.io/_uploads/B1o1l3Jgex.png)

Algorytm Bellmana-Forda pozwala nam jednak wyznaczyć najkrótszą ścieżkę i jednocześnie wykryć ujemny cykl. Dla każdego wierzchołka będziemy przechodzić wszystkimi ścieżkami i poprawiać, jeśli znaleźliśmy lepszy wynik.

Na początku ustawimy wszystkie odległości wierzchołków jako nieskończoność.

![image](https://hackmd.io/_uploads/S1fml3Jlll.png)

Odwiedzimy sąsiadów wierzchołka startowego i ustalimy nowe wartości.

![image](https://hackmd.io/_uploads/rk0meh1lxl.png)

Okazuje się, że odwiedzony wcześniej wierzchołek ma lepszą wartość od sąsiada niż z wierzchołka startowego, więc ją aktualizujemy.

![image](https://hackmd.io/_uploads/rJu4l2Jxee.png)

Na końcu otrzymujemy:

![image](https://hackmd.io/_uploads/rkrrehJlgg.png)

Algorytm wygląda zatem w taki sposób:

```plaintext
 wykonuj w kółko
  weź każdą krawędź l = (u, v)
   DST[v] = min(DST[v],DST[u]+w(l))
```

Kiedy ten program się skończy? Wywołujemy się na każdym wierzchołku, więc wszystkie wartości będą ustawione poprawnie po $n$ iteracjach dla $n$ wierzchołków, gdyż ścieżki w grafie nie mogą mieć większej długości (o ile nie przechodzą przez jakieś wierzchołki wielokrotnie). 
Da się to trochę lepiej oszacować.  Wystarczy zauważyć, że cały graf będzie miał aktualne dane już po przejściu najdłuższej spośród optymalnych ścieżek z wierzchołka $u$. 

**W jaki sposób wykryć ujemny cykl?** Zauważyliśmy, że $n$ iteracji jest górną granicą. Z tego wynika, że jeśli nasze wartości zmienią się po $n+1$ iteracji, to mamy ujemny cykl.

## Algorytm Floyda-Warshalla

Jest to drugi sposób na rozwiązanie wcześniejszego zadania.

```plaintext
dla v = 1, ..., n
  dla u = 1, ..., n
    dla w = 1, ..., n
      DST[u][w] = min(DST[u][w], DST[u][v] + DST[v][w])
```

Dla potrzeby tego algorytmu trzymamy graf jako macierz sąsiedztwa, czyli tablicę $\text{DST}[n][n]$:

![image](https://hackmd.io/_uploads/H17PZhkxll.png)

|   | 1 | 2 | 3 | 4 | 5 |
|---|---|---|---|---|---|
| 1 | 0 | 1 | 5 | $\infty$ | $\infty$ |
| 2 | 1 | 0 | $\infty$ | 2 | $\infty$ |
| 3 | 5 | $\infty$ | 0 | 3 | 2 |
| 4 | $\infty$ | 2 | 3 | 0 | 7 |
| 5 | $\infty$ | $\infty$ | 2 | 7 | 0 |


Teraz dla $u = 2$ i $w = 3$ będziemy analizować ścieżkę, przechodząc przez wierzchołek pośredni $v=1$. Widać, że $1 + 5$ będzie lepsze niż $\infty$, więc aktualizujemy wartość:

|   | 1 | 2 | 3 | 4 | 5 |
|---|---|---|---|---|---|
| 1 | 0 | 1 | 5 | $\infty$ | $\infty$ |
| 2 | 1 | 0 | **6** | 2 | $\infty$ |
| 3 | 5 | **6** | 0 | 3 | 2 |
| 4 | $\infty$ | 2 | 3 | 0 | 7 |
| 5 | $\infty$ | $\infty$ | 2 | 7 | 0 |


Dla $v=2$, $u = 1$, $w = 4$:

|   | 1 | 2 | 3 | 4 | 5 |
|---|---|---|---|---|---|
| 1 | 0 | 1 | 5 | **3** | $\infty$ |
| 2 | 1 | 0 | 6 | 2 | $\infty$ |
| 3 | 5 | 6 | 0 | 3 | 2 |
| 4 | **3** | 2 | 3 | 0 | 7 |
| 5 | $\infty$ | $\infty$ | 2 | 7 | 0 |


Na sam koniec otrzymujemy:

|   | 1 | 2 | 3 | 4 | 5 |
|---|---|---|---|---|---|
| 1 | 0 | 1 | 5 | 3 | 7 |
| 2 | 1 | 0 | 5 | 2 | 9 |
| 3 | 5 | 5 | 0 | 3 | 2 |
| 4 | 3 | 2 | 3 | 0 | 5 |
| 5 | 7 | 9 | 2 | 5 | 0 |


Jak udowodnić poprawność tego algorytmu? Weźmy pewną **optymalną** ścieżkę przechodzącą przez wierzchołki:

$5 - 1 - 4 - 3 - 7 - 6 - 9$.

Zauważmy, żę jeśli powyższa ścieżka jest optymalna, to wszystkie jej fragmenty (np. $4 - 3 - 7 - 6$) też muszą być optymalne. To samo tyczy się w szczególności użytych powyżej krawędzi, które są na początku ustawione poprawnie w naszym algorytmie.

Zauważmy, że mając optymalną wartość ścieżki z $5$ do $1$ oraz z $1$ do $4$, po rozpatrzeniu $v=1$ nasz algorytm ustawi na optymalną wartość ścieżki z $5$ do $4$. 

Co dalej zrobi nasz algorytm dla tej przykładowej ścieżki?

- dla $v=1$ wyznaczy optylalnie $(5,4)$,
- dla $v=2$ nic, bo nasza ścieżka nie  zawiera wierzchołka $2$,
- dla $v=3$ wyznaczy optymalnie $(4,7)$,
- dla $v=4$ wyznaczy optymalnie $(1,3)$, ale też za pomocą wyliczonych wcześniej $(5,4)$ i $(4,7)$ znajdzie optymalną wartość  $(5,7)$,
- itd., aż wyznaczymy $(5,9)$.

Skoro budujemy nową ścieżkę z dwóch mniejszych optymalnych ścieżek, to musi być optymalna.

# Wykład 10: Grafy

## Sortowanie topologiczne

:::success
Zadanie. Dany jest acykliczny graf skierowany. Wyznacz jego uporządkowanie topologiczne.
:::

:::info
Definicja. Porządek topologiczny, to taki, w którym dla każdej krawędzi $u \rightarrow v$ wierzchołek $u$ stoi przed $v$.
:::

Na przykładzie:

![image](https://hackmd.io/_uploads/HJj4zsQZxe.png)

Mamy taki porządek:

1 4 2 3 7 8 5 6

Można zauważyć, że to nie jedyna kombinacja, która spełnia zależność. Mamy też:

* 4 1 2 3 7 8 5 6
* 1 4 2 7 8 3 5 6
* ogólnie różne permutacje między łańcuchami 1 2 3 6, 1 7 8 5 oraz 4 5

Możemy to rozwiązać na dwa sposoby.

### Lista do odwiedzenia

Będziemy wybierać po kolei takie wierzchołki, do których nie da się dojść i usuwać ich krawędzie wychodzące.

Dla przykładu:

1. Wybierzemy jako pierwszy wierzchołek $1$ lub $4$, bo żadne krawędzie do nich nie prowadzą. Dodajemy je do listy.

![image](https://hackmd.io/_uploads/BkY_GjX-ee.png)

2. Z naszej listy do odwiedzenia $[1;4]$ wybierzemy $1$. Wykreślimy teraz wszystkie krawędzie tego wierzchołka i zobaczymy, czy są nowe, do których nie da się przejść. 

![image](https://hackmd.io/_uploads/BJcIGjmZex.png)

3. Poprzedni krok dodaje nam do listy $[2;7]$, czyli mamy z poprzednim stanem $[4;2;7]$. Stąd wybierzemy kolejny wierzchołek tak, jak wcześniej, dopóki lista nam się nie wyczyści.

Co jeśli interesowałoby nas najmniejsze leksykograficznie rozwiązanie?
Wystarczyłoby zamiast wybierać losowy wierzchołek, brać zawsze ten najmniejszy na liście.

### DFS

Zwykły DFS też rozwiązuje nam problem. Wystarczy zapamiętać wierzchołek, gdy wychodzimy z rekurencji.

Dla przykładu ze wcześniejszym grafem:

* Załóżmy, że DFS przeszedł już taką ścieżkę.
 
 ![image](https://hackmd.io/_uploads/r1J3fs7bge.png)
 
* Teraz będzie wychodzić z rekurencji, bo nigdzie dalej nie pójdzie, więc wypisze nam $6$.
* Dalej wypisze nam $3$ oraz $2$, a następnie wywoła się na kolejnych odnogach $1$.

Uzyskujemy coś takiego:

6 3 2 5 8 7 1 4

A to jest oczywiście odwrotne do tego, co chcieliśmy uzyskać. Wystarczy wypisać od końca.

---

## Silnie spójne składowe

:::success
Zadanie. Dany jest graf skierowany. Wyznacz w nim takie zbiory, że każdy tworzy jedną maksymalną silnie spójną składową.
:::

:::info
Definicja. Silnie spójna składowa to grupa wierzchołków w grafie skierowanym, gdzie każdy jest osiągalny z każdego innego.
:::

Na przykład:

![image](https://hackmd.io/_uploads/SkJ0MoX-xx.png)

### Pierwsza obserwacja

Jeśli występuje silna spójna składowa, to nie istnieje sortowanie topologiczne. Dlaczego? Zobaczmy, co zrobią nasze poprzednie algorytmy na takim grafie.

![image](https://hackmd.io/_uploads/BJFCMombgl.png)

1. Algorytm z listą zatka się już na samotnym wierzchołku $11$, gdyż nie ma innego wierzchołka o stopniu wejściowym 0.
2. DFS wypisze nam 9 8 11 1 2 4 3 10 7 6 5, cokolwiek to znaczy.

Weźmy teraz wszystkie silnie spójne składowe we wcześniejszym grafie i zróbmy z nich duże wierzchołki.

![image](https://hackmd.io/_uploads/Syvk7iQWel.png)

Zobaczmy jeszcze raz na wynik wcześniejszego DFS-a:

9 8 11 1 2 4 3 10 7 6 5

Zaznaczmy teraz na nim te silne spójne składowe, które są obok siebie. Można zauważyć, że większość z nich znalazł poprawnie:

(9) (8) (11) 1 (2) (4 3 10) 7 6 (5)

Jedynie rozdzielona jest (1 7 6).

### Druga obserwacja

Co się stanie, jeśli odwrócimy wszystkie krawędzie w grafie?

![image](https://hackmd.io/_uploads/rkXfQjmZll.png)

Można zauważyć, że silnie spójne składowe zostają bez zmian. Skoro mogliśmy z każdego wierzchołka $u$ przejść do $v$ i z powrotem, to oczywiście po odwróceniu krawędzi dalej możemy. Natomiast, jeśli rozpatrzymy wierzchołki w odpowiedniej kolejności,  każde inne miejsce się blokuje.

### Algorytm wyznaczania silnie spójnych składowych

Korzystając z tych dwóch obserwacji, możemy skonstruować algorytm:

1. Uruchamiamy DFS z sortowania topologicznego.
2. Odwracamy wszystkie krawędzie w grafie.
3. Uruchamiamy zwykły DFS na wierzchołkach w kolejności z kroku pierwszego. Jeśli możemy iść dalej niż jeden wierzchołek, to wykreślamy go z kolejności, bo wtedy znajdujemy spójną składową większą niż $1$.

**Dlaczego to działa?**

Przyjrzyjmy się kolejności wygenerowanej przez DFS, a dokładniej *pierwszym odwiedzonym wierzchołkom dla każdej spójnej składowej*. W naszym grafie są to 9, 8, 11, 1, 2, 4, 5. Zauważmy, że kolejność tych wierzchołków odpowiada kolejności topologicznej reprezentowanych przez nie spójnych składowych.
To spostrzeżenie można uzasadnić tak, że jeśli już wejdziemy do jednego z wyżej wymienionych wierzchołków, to DFS odwiedzi wszystkie osiągalne z niego wierzchołki (w szczególności wszystke spójne składowe) zanim z niego wyjdzie. Zatem spełniają one dokładnie tą samą własność, co sortowanie topologiczne używające DFSa.

Zatem, jeśli pomyślimy o silnych spójnych składowych jak o dużych wierzchołkach o odpowiednich reprezentantach, możemy uzyskać sortowanie topologiczne po zwykłym usunięciu nadmiaru z silnych spójnych składowych.

Przejdźmy do algorytmu.

* Krok pierwszy po prostu przechodzi po grafie i zapisuje kolejność.
* W wyniku kroku drugiego zablokujemy dalszą trasę dla wierzchołków, ale dla silnych spójnych składowych nie będzie różnicy.
* Przechodząc jeszcze raz według poprzedniej kolejności, powinniśmy być w tych wierzchołkach, z których wcześniej mogliśmy iść dalej. Jednak po odwróceniu nie przejdziemy dalej, chyba że trafiamy na jakiś cykl.




---

## Cykl Eulera

:::success
Zadanie. Dany jest graf. Wyznacz dla niego cykl Eulera.
:::

:::info
Definicja. Cykl Eulera to przejście po wszystkich krawędziach dokładnie raz, z wierzchołka $v$ do $v$.
:::

Przykład:

![image](https://hackmd.io/_uploads/SyskVs7Zlg.png)

Cykl Eulera dla tego grafu to na przykład 1 3 4 5 6 8 9 7 6 3 7 5 3 2 1.

### Jakie grafy mają cykl Eulera?

Żeby graf miał cykl Eulera, musi spełniać pewne warunki:
* musi być **spójny** (co najmniej części, które mają jakieś krawędzie),
* **Graf nieskierowany:**
  * każdy wierzchołek musi mieć **parzysty stopień** (bo do każdego wchodzimy i wychodzimy).
* **Graf skierowany:**
  * każdy wierzchołek musi mieć tyle samo wejść, co wyjść.

### Zwykły DFS

Weźmy taki graf i przejdźmy po nim DFS-em.

![image](https://hackmd.io/_uploads/SyRXVoQbgl.png)

Widać, że to zadziała — po prostu pójdzie w taki sposób:

1 2 3 4 5 6 7 4 2 8 1

Ale co, jeśli DFS pójdzie z wierzchołka 2 do 8 zamiast do 3? Wtedy dostaniemy tylko:

1 2 8 1

Można zauważyć, że tę sekwencję już mieliśmy wcześniej:

**1 2** 3 4 5 6 7 4 **2 8 1**

DFS się jednak nie zatrzyma — pójdzie dalej przez $3$. Wystarczy później **połączyć cykle Eulera**.

### DFS z sortowania topologicznego

Łatwiej jest wypisywać wierzchołki dopiero wtedy, kiedy się cofamy.

Przykład:
1 8 2 3 4 7 6 8 7 2 1

Dostajemy wynik w odwrotnej kolejności, ale to nie robi różnicy w grafie nieskierowanym.

### Ścieżka Eulera

Pomysł jest prawie taki sam, jednak teraz początkowy i końcowy wierzchołek muszą spełniać inny warunek:

* mają **nieparzysty stopień wejścia i wyjścia** (czyli różnią się o $1$),
* pozostałe wierzchołki mają równe liczby wejść i wyjść.

### Wskazówka implementacyjna

Co jeśli graf jest nieskierowany?

Zauważmy, że reprezentujemy go jako dwie krawędzie skierowane: $a \rightarrow b$ oraz $b \rightarrow a$.

A wtedy w algorytmie trzeba pozbyć się **drugiej strony**, kiedy wybraliśmy już jakiś kierunek.

Można to zrealizować np. trzymając dodatkową tablicę globalną i odznaczając, czy dana krawędź została już użyta.

# Wykład 11: Geometria

## Iloczyn wektorowy

:::success
Zadanie. Dane są dwa punkty na płaszczyźnie $A(x_A, y_A), B(x_B, y_B)$, gdzie prosta przechodząca przez środek układu i punkt $A$ tworzy z osią $OX$ kąt $\alpha$, a $B$ tworzy kąt $\beta$. Czy $\alpha > \beta$?
:::

![image](https://hackmd.io/_uploads/ByMhA0h-el.png)

Zacznijmy od czegoś prostszego. Jak sprawdzić, czy dwa punkty leżą na tej samej prostej?

![image](https://hackmd.io/_uploads/Hye94yyaWge.png)

Jeszcze ze szkoły średniej wiemy, że proste są równoległe wtedy i tylko wtedy, gdy dla prostych w postaci ogólnej

* $A_1x + B_1y + C_1 = 0$ oraz 
* $A_2x + B_2y + C_2 = 0$

zachodzi:

$$
A_1 \cdot B_2 - A_2 \cdot B_1 = 0
$$

Wprost z tego faktu wynika, że punkty $A$ i $B$ leżą na tej samej prostej wtedy i tylko wtedy, gdy:

$$
x_B \cdot y_A - x_A \cdot y_B = 0
$$

Dokładnie to jest **iloczyn wektorowy** $W(A,B)$. Dodatkowo wiemy o nim kilka przydatnych rzeczy:

* $W(A,B) = 0$ → punkty $A$ i $B$ leżą na tej samej prostej względem punktu $(0,0)$
* $W(A,B) > 0$ → $\alpha > \beta$
* $W(A,B) < 0$ → $\alpha < \beta$

A jeśli dodatkowo w zadaniu dostalibyśmy punkt $P$, względem którego mielibyśmy wyliczyć, który kąt jest większy?

![image](https://hackmd.io/_uploads/BJIwkyTWxg.png)

Wystarczy przesunąć wszystko o wektor $P$, wtedy będziemy znów w punkcie $(0,0)$, czyli obliczamy $W(A - P, B - P)$.

---

## Pole trójkąta

:::success
Zadanie. Dane są punkty $A(x_A, y_A), B(x_B, y_B)$. Oblicz pole trójkąta $AOB$, gdzie $O$ to punkt $(0,0)$.
:::

Korzystając z iloczynu wektorowego, mamy:

$$
P_{AOB} = \frac{|W(A,B)|}{2}
$$

**Dowód:**

![image](https://hackmd.io/_uploads/r1DCk1abeg.png)

Z klasycznego wzoru:

$$
P_{AOB} = \frac{a \cdot b \cdot \sin \gamma}{2}
$$

Ale $\gamma = \alpha - \beta$, więc:

$$
\begin{align*}
P_{AOB} &= \frac{a \cdot b \cdot |\sin (\alpha - \beta)|}{2} = \frac{ab |\sin \alpha \cos \beta - \sin \beta \cos \alpha|}{2}\\
&= \frac{|\sin \alpha \cdot a \cdot \cos \beta \cdot b - \sin \beta \cdot b \cdot \cos \alpha \cdot a|}{2}
\end{align*}
$$

Dodatkowo wiemy, że:

* $x_A = \cos \alpha \cdot a$
* $y_A = \sin \alpha \cdot a$
* $x_B = \cos \beta \cdot b$
* $y_B = \sin \beta \cdot b$

czyli po podstawieniu:

$$
P_{AOB} = \frac{|y_A \cdot x_B - y_B \cdot x_A|}{2} = \frac{|W(A,B)|}{2}
$$

---

## Pole wielokąta

:::success
Zadanie. Danych jest $N$ punktów $P_1, ..., P_N$ oznaczających kolejne wierzchołki wielokąta wypukłego. Oblicz jego pole.
:::

Jednym z rozwiązań jest podzielić wielokąt na trójkąty, wybierając do tego jeden z wierzchołków, i obliczać kolejne pola z pozostałymi.

Dużo ciekawszą obserwacją jest rozważenie przypadku, gdy wybierzemy inny punkt — na przykład $(0,0)$. Jak policzyć pole figury dla takiego przypadku?

Weźmy wielokąt i zaznaczmy pola trójkątów z wierzchołków do punktu $(0,0)$.

![image](https://hackmd.io/_uploads/Bk0egyTWee.png)

Zaznaczmy teraz pola nieco inaczej.

![image](https://hackmd.io/_uploads/HypbxJ6Wxx.png)

Można zauważyć, że jeśli niektóre pola dodamy, a niektóre odejmiemy, to dostaniemy w wyniku to, czego potrzebujemy. Jak rozpoznać pola, które powinny zostać odjęte?

Każde pole, które powinniśmy odjąć, to takie, gdzie przy następnym punkcie kąty **maleją**.

Przypomnijmy sobie, co robił **iloczyn wektorowy**:

* $W(A,B) > 0$ → $\alpha > \beta$, czyli wtedy, kiedy kąty **rosną**
* $W(A,B) < 0$ → $\alpha < \beta$, czyli wtedy, kiedy kąty **maleją**

Dodatkowo wcześniej ustaliliśmy, że **iloczyn wektorowy** to również wartość pola trójkąta między dwoma punktami a początkiem układu współrzędnych.

**Podsumowując**, aby policzyć pole takiego wielokąta, wystarczy:

$$
P = \frac{ |\sum_{i=1}^n W(P_i, P_{i+1})| }{2}
$$

**Dlaczego?**
Dobre pola się dodają, bo iloczyn wektorowy jest dodatni, a złe pola się odejmują, bo iloczyn wektorowy jest ujemny.

A co dla innych wielokątów niż wypukłe? Dokładnie to samo. Zauważmy, że podzielenie innych wielokątów niż wypukłe na trójkąty z jakiegoś wierzchołka byłoby problematyczne, natomiast ta metoda jest niezawodna. Na przykładzie:

![image](https://hackmd.io/_uploads/Sk3nlkabee.png)

Znowu złe pola to te, w których maleją kąty, czyli jeśli po prostu zsumujemy wszystkie pola, złe się same zredukują.

---

## Otoczka wypukła

:::success
Zadanie. Danych jest $N$ punktów na płaszczyźnie. Znajdź ich otoczkę wypukłą.
:::

:::info
Definicja. Otoczka wypukła to wielokąt wypukły składający się z wierzchołków z danego zbioru i zawierający wewnątrz pozostałe.
:::

Na przykładzie:

![image](https://hackmd.io/_uploads/BJeeZkp-ge.png)

Otoczka wypukła tego zbioru to:

![image](https://hackmd.io/_uploads/ry4zZkTbex.png)

### Sortowanie kątowe

:::info
Definicja. Sortowanie kątowe to taka permutacja punktów $P_1, ..., P_N$, że każdy iloczyn wektorowy $W(P_i, P_{i+1}) > 0$ względem punktu $(0,0)$.
:::

Weźmiemy stos i punkty posortowane kątowo:

* dodajemy do stosu kolejny punkt z sortowania
* sprawdzamy, czy wciąż idziemy w **lewo** (z perspektywy wektora), czyli $W(P_i, P_{i+1}) < 0$
* jeśli nowo dodany punkt nie spełnił warunku, to usuwamy ze stosu wszystkie punkty, które z nim kolidują

Dla przykładu:

Zaczniemy od wyboru numeru 1. **Stos: 1**.

![image](https://hackmd.io/_uploads/SkhSbJablx.png)

Dodamy bez problemu do stosu 2, 3, 4. **Stos: 1, 2, 3, 4**. 

Gdy chcemy dodać numer 5, zauważymy, że nie pasuje to do 4, więc 4 zostanie usunięta. **Stos: 1, 2, 3, 5**.

![image](https://hackmd.io/_uploads/r1xGGkT-le.png)

Podobnie przy próbie dodania 6, usuniemy 5. **Stos: 1, 2, 3, 6**.

![image](https://hackmd.io/_uploads/H1xrz1Tbxe.png)

Należy pamętać, że musimy usuwać wszystkie wierzchołki, które nie pasują. Gdybyśmy mieli taką sytuację:

![image](https://hackmd.io/_uploads/HktKM1p-ee.png)

Przy próbie dodania 9 usuniemy dwie wartości ze stosu.

![image](https://hackmd.io/_uploads/Hyvoz16ble.png)

---

### Otoczka górna i dolna

Innym sposobem jest skorzystanie ze zwykłego sortowania dla par, czyli sortowania punktów względem współrzędnej x, a w przypadku równości względem współrzędnej y.
Idąc od początku z tym samym algorytmem co w pierwszym sposobie, dostajemy **górną otoczkę** — usuniemy ze stosu dolne wartości.

![image](https://hackmd.io/_uploads/BJJCfJpbee.png)

Idąc od końca do początku, dostaniemy **dolną otoczkę**.
Wystarczy połączyć dwa wyniki w jeden.

---

# Wykład 12: Haszowanie

## Funkcja haszująca

Szukamy takiej funkcji $f: \text{string} \rightarrow \text{int}$, która dla każdego słowa przyporządkuje nam pewną liczbę.

### Kolizje

Typ $\text{int}$ jest całkiem spory, jednak do zakodowania wszystkich słów - zdecydowanie niewystarczający. Weźmy sobie **tylko 50-literowe** słowa. Kombinacji mamy $26^{50}$ oraz około $10^9$ możliwych unikatowych identyfikatorów.

Szukamy więc funkcji, która **minimalizuje liczbę kolizji**, czyli przypadków, w których różne słowa mają taki sam hasz.

### Przykłady

Rozważmy kilka funkcji hashujących:
* $f(s) = 1$ - nie nadaje się, wszędzie mamy kolizje, nic sensownego nie koduje
* $f(s) = \text{rand()}$ - to samo słowo będzie mieć różne wartości
* $f(s) = \sum \text{s[i]}$, gdzie przypiszemy $$a \rightarrow 0, \quad b \rightarrow 1, \quad ..., \quad z \rightarrow 25$$
	 Teraz jest trochę lepiej, ale już w 2-literowych słowach mamy kolizje $f(ab) = f(ba)$.
* $f(s) = (\sum \text{s[i]} \cdot A^i) \% M$, gdzie $A = 32$, $M = 2^{32}$
  Co w tym przypadku jest nie tak? 
  
  Dla słów długości co najmniej 7, wyższe znaki  będziemy mnożyć przez wartość $2^{35}$, co po nałożeniu modulo będzie dawać 0. Zatem mamy kolizję na przykład $f(abcdefg)=f(abcdefghi)$. 
* $f(s) = (\sum \text{s[i]} \cdot A^i) \% M$, gdzie $A = 34$, $M = 10^9$ 
  
  Czy ta funkcja jest już dobra? Lepsza niż poprzednie, ale wciąż można ją poprawić. $M$ nie jest pierwsza, więc mnożniki dla wyższych liter będą podzielne przez $2^9$.
  
* $f(s) = (\sum \text{s[i]} \cdot A^i) \% M$, gdzie $A = 10$, $M = 10^9 + 7$ 
  
  Tym razem problemem jest $A$. Dlaczego?
  $f(bb) = 1 \cdot 1 + 1\cdot 10 = 1 \cdot 11 = f(l)$. Nasze $A$ jest mniejsze od ilości liter w alfabecie.
  
* $f(s) = (\sum \text{s[i]} \cdot A^i) \% M$, gdzie $A = 37$, $M = 10^9 + 7$
	 
	 Została ostatnia rzecz do poprawy. Ustaliliśmy, że $a \rightarrow 0$, czyli inaczej ujmując, doklejenie $a$ do naszego słowa nic nie zmienia. Np. $f(b) = f(ba)$
	 
	 Wystarczy ustalić, że 
	 
	 $$a \rightarrow 1, \quad b \rightarrow 2, \quad ..., \quad z \rightarrow 26$$
	 
	 Teraz nasza funkcja jest już całkiem porządna.

### Prawdopodobieństwo kolizji

Załóżmy, że prawdopodobieństwo kolizji wynosi $\frac{1}{M}$.

1. **Jakie jest prawdopodobieństwo, że dwa elementy ($x$, $y$) nie mają kolizji?**
   Wybieramy element $x$, a na kolizję z $y$ mamy z założenia szansę $\frac{1}{M}$. Dlatego na **brak kolizji** mamy szanse 

$$1 - \frac{1}{M}$$

2. **Jakie jest prawdopodobieństwo, że $x$ nie ma kolizji z żadnym z $y₁, ..., y_k$?**

   Dla każdego mamy szansę, by uniknąć kolizji $\frac{1}{M}$. Ale tych par (niekoniecznie unikatowych) mamy $k$.

$$ \left(1 - \frac{1}{M} \right)^k$$

3. **Jakie jest prawdopodobieństwo braku kolizji w zbiorze $\{x_1, ..., x_k\}$?**
  Pierwszy element wybieramy z szansą na brak kolizji 1, bo nie ma z czym kolidować. Drugi będzie miał już szansę $\frac{1}{M}$. Następna wartość może już z dwoma skolidować, więc $\frac{2}{M}$.

$$
1 \left(1 - \frac{1}{M} \right) \left(1 - \frac{2}{M} \right) \cdots \left(1 - \frac{k-1}{M} \right) = \prod_{i=0}^{k-1} \left(1-\frac{i}{M} \right)
$$

---

### Jak wybrać odpowiednie $M$?

Najlepiej przetestować. Przykłady dla zbioru o $k = 10^6$ elementach:

|     $M$     | Pr. braku kolizji dla pary | Pr. braku kolizji dla $k$ elementów | Pr. braku kolizji w całym zbiorze |
| --------- | -------------------- | ------------------------- | --------------------------------- |
| $10^6$    | $1 - 10^{-6}$        | $0,367$                   | $0$                               |
| $10^9$    | $\approx 1$          | $0,999$                   | $10^{-200} \approx 0$                       |
| $10^{12}$ | $\approx 1$          | $\approx 1$               | $0,606$                           |
| $10^{18}$ | $\approx 1$          | $\approx 1$               | $0.9999995$                       |

Dla większości przypadków wystarczy $M = 10^9$.
Jeśli zależy nam na bardzo małym prawdopodobieństwie kolizji — warto użyć **podwójnego haszowania**:

Wtedy nasze prawdopodobieństwo jest porównywalne z przypadkiem $M = 10^{18}$.

## Obliczanie haszy

Dzięki strukturze funkcji haszującej można szybko wyznaczyć wartość dłuższych słów na podstawie krótszych.

Weźmy słowo $s$ o długości $n$.

$f(s) = (s[1] \cdot A + s[2] \cdot A^2 + \ldots + s[n] \cdot A^n) \% M$

Aby obliczyć hasz słowa $s$ możemy też wykorzystać słowo krótsze o ostatnią literę:

$f(s) = (f(s[1,...,n-1]) + s[n] \cdot A^n) \% M$

A jak obliczyć tylko fragment $[i,j]$ w słowie? Oznaczmy $s[i,j]$ jako fragment słowa $s$ od indeksu $i$ do $j$ włącznie.

Możemy rozbić problem na dwie części:

$f(s[1,j]) = (s[1] \cdot A + s[2] \cdot A^{2} + \ldots + s[j] \cdot A^j) \% M$

$f(s[1,i-1]) = (s[1] \cdot A + s[2] \cdot A^2 + \ldots + s[i-1] \cdot A^{i-1}) \% M$

Mamy policzone prefiksy $[1, i-1]$ oraz $[1, j]$.

$f(s[i,j]) = \sum_{k=i}^{j} s[k] \cdot A^{k-i+1} \% M = \frac{f(s[1, j]) - f(s[1,i-1])}{A^{i-1}} \% M$

**Uwaga!** Dzielimy modularnie. Istnieją jednak sposoby, żeby uniknąć tego dzielenia, na przykład jeśli przyjmiemy, że pierwsza litera ma najwyższą potęgę $A$, a ostatnia jest mnożona przez jedno $A$.

---

## Porównanie zbiorów liczb

:::success
Zadanie. Dane są dwa ciągi liczb i dwa przedziały. Czy w danym przedziale występują te same wartości?
:::

Do rozwiązania tego zadania wystarczy prosta funkcja haszująca.

$0 \rightarrow 17453,\quad 1 \rightarrow 29247,\quad \ldots,\quad 9 \rightarrow 18835$

Wartości są zupełnie losowe, jednak mają pewną ważną zaletę — są znacznie większe od liczb z przedziału $[0,9]$. Dzięki temu wystarczy funkcja haszująca oparta na przykład na  **sumie** bądź operacji xor. Istnieje duża szansa, że nie wystąpi kolizja. Wystarczy zatem porównać wartości funkcji haszującej dla przedziałów w obu zbiorach.

---

## Hasze prefiksowe

:::success
Zadanie. Dane są dwa słowa — tekst $T$ o długości $n$ oraz wzorzec $W$ o długości $m$. Znajdź wzorzec w tekście.
:::

Sztuczką w tym zadaniu będzie odpowiedni **preprocessing**. Załóżmy, że mamy już policzone wartości odwrotności modularnych oraz wszystkie prefiksowe wartości funkcji haszującej dla tekstu $T$.

Co możemy z tym zrobić? Prefiksowe hasze pozwalają obliczyć wartość funkcji haszującej dla każdego fragmentu $T[i,i+m-1]$ w czasie stałym. Wystarczy teraz, dla każdej pozycji w tekście, sprawdzić, czy hasz dla fragmentu jest równy haszowi wzorca.

---

## Porównanie leksykograficzne

Normalnie, aby porównać dwa słowa leksykograficznie, sprawdzamy kolejne znaki, aż do momentu różnicy, a następnie porównujemy różniące się litery. Teraz zrobimy to samo, ale szybciej, wykorzystując funkcję haszującą.

Mając wcześniej obliczone hasze prefiksowe, możemy błyskawicznie sprawdzić, czy dany fragment jest równy. Wystarczy użyć wyszukiwania binarnego, aby znaleźć największy wspólny prefiks. Następnie sprawdzamy tylko pierwszą różniącą się literę.

---

## Najmniejszy leksykograficznie sufiks

:::success
Zadanie. Dane jest słowo. Podaj najmniejszy leksykograficznie sufiks.
:::

Dla przykładu:

* $cdag \to ag$
* $abagcd \to abagcd$

W tym zadaniu wystarczy wykorzystać $n$ razy funkcję do porównywania leksykograficznego dla każdego sufiksu.

**Wskazówka implementacyjna.** Pomocne może się okazać dodanie na końcu słowa wartownika.

---

## Najdłuższy palindrom

:::success
Zadanie. Dane jest słowo $T$ o długości $n$. Znajdź najdłuższy palindrom.
:::

Wyszukiwanie najdłuższego wspólnego prefiksu możemy wykorzystać również w tym zadaniu. Dla każdej pozycji $i$:

* znajdź najdłuższy fragment taki, że hasz prefiksowy jest równy haszowi sufiksowemu — zastosuj wyszukiwanie binarne,
* rozważ dwa przypadki: palindromy o długości parzystej i nieparzystej — obsłuż je osobno.

# Wykład 13: Algorytmy tekstowe

## Wyszukiwanie wzorca (KMP)

:::success
Zadanie. Dane są słowa $W$, $T$. Sprawdź, czy $W$ występuje w $T$.
:::

**Algorytm KMP** pozwala wyznaczyć wszystkie wzorce w tekście w złożoności liniowej wykorzystując **prefikso-sufiksy**.

### Prefikso-sufiksy

:::info
Definicja. Prefikso-sufiks słowa, to podsłowo, które jest jednocześnie jego prefiksem i sufiksem, a przy tym jest krótsze niż całe słowo.
:::

Na przykład:

- $abcdab \rightarrow ab$
- $abababcababa \rightarrow a, aba, ababa$

Dwa trywialne przypadki: słowo puste i całe słowo są prefiksami i sufiksami danego słowa. W naszych rozważaniach nie będziemy traktowali całego słowa jako prefikso-sufiksu.

W algorytmie KMP będziemy potrzebowali znać prefikso-sufiksy każdego prefiksu. Dla przykładu:

Mamy dane słowo **abacababaabacaa**. Dla prefiksu **a** nie ma prefikso-sufiksu innego niż całe słowo, więc pomijamy ten przypadek.

| a | b | a | c | a | b | a | b | a | a | b | a | c | a | a |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 0 |   |   |   |   |   |   |   |   |   |   |   |   |   |   |

Dla **ab** też nie ma prefikso-sufiksu.

| a | b | a | c | a | b | a | b | a | a | b | a | c | a | a |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 0 | 0 |   |   |   |   |   |   |   |   |   |   |   |   |   |

Dla **aba** mamy prefikso-sufiks **a** o długości 1.

| a | b | a | c | a | b | a | b | a | a | b | a | c | a | a |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 0 | 0 | 1 |   |   |   |   |   |   |   |   |   |   |   |   |

Dla **abac** nie ma prefikso-sufiksu.

| a | b | a | c | a | b | a | b | a | a | b | a | c | a | a |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 0 | 0 | 1 | 0 |   |   |   |   |   |   |   |   |   |   |   |

Resztę analogicznie:

| a | b | a | c | a | b | a | b | a | a | b | a | c | a | a |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 0 | 0 | 1 | 0 | 1 | 2 | 3 | 2 | 3 | 1 | 2 | 3 | 4 | 5 | 1 |

**Obserwacje:**

- Po każdej literce długość prefikso-sufiksu zwiększy się nam maksymalnie o **1**.

  Mamy jakiś prefikso-sufiks $s$ o długości 4 oraz literkę $x$. Jeśli w słowie po $s$ jest $x$, to dodając je na koniec zwiększamy o 1.

  | słowo | $s$ | $x$ | ... | $s$ | $x$ |
  |-------|-----|-----|-----|-----|-----|
  | długość | 4 | 1 | ... | 4 | 1 |

  Czyli dla słowa $t = s + x$:

  | słowo | $t$ | ... | $t$ |
  |-------|-----|-----|-----|
  | długość | 5 | ... | 5 |

- Co jeśli następna literka $y$ jest inna niż $x$?

  | słowo | $s$ | $y$ | ... | $s$ | $x$ |
  |-------|-----|-----|-----|-----|-----|
  | długość | 4 | 1 | ... | 4 | 1 |

  Zauważmy, że słowo $s$ też ma swój największy prefikso-sufiks $p$.

  | słowo $s$ | $p$ | ... | $p$ |
  |-----------|-----|-----|-----|

  Do tego $p$ jest na początku i na końcu. Wystarczy w takim razie zobaczyć, co się dzieje obok $p$.

  | słowo $s$ + $x$ | $p$ | $z$ | ... | $p$ | $x$ |
  |-----------------|-----|-----|-----|-----|-----|

  Tutaj wyłania się właśnie ważna zaleźność.

**Symulacja:**

Załóżmy, że musimy uzupełnić poprawnie ostatnią wartość.

| a | b | a | c | a | **b** | a | b | a | a | b | a | c | **a** | a |
|---|---|---|---|---|-------|---|---|---|---|---|---|---|-------|---|
| 0 | 0 | 1 | 0 | 1 | 2     | 3 | 2 | 3 | 1 | 2 | 3 | 4 | **5** | ? |

Widać, że a $\neq$ b, więc przeniesiemy nasz problem do prefikso-sufiksu **abaca**.

| a | b | a | c | a | **b** | ... | a | b | a | c | a | **a** |
|---|---|---|---|---|-------|-----|---|---|---|---|---|-------|
| 0 | 0 | 1 | 0 | 1 | 2     | ... | 1 | 2 | 3 | 4 | 5 | ?     |

Znów mamy a $\neq$ b, więc zapytamy o prefikso-sufiks **a**.

| **a** | ... | **a** |
|-------|-----|-------|
| 0     | ... | 1     |

A tutaj już mamy, że a = a, więc przedłużamy prefikso-sufiks długości 0 o 1.

### Standardowe wyszukiwanie

Przygotujemy teraz tablicę prefikso-sufiksów dla wzorca. Będziemy przeszukiwać tekst prawie tak samo, jak w rozwiązaniu brutalnym, ale z usprawnieniem, gdy litery się nie zgadzają.

W rozwiązaniu brutalnym przechodziliśmy po każdym indeksie i porównywaliśmy całe słowo od nowa. Teraz będziemy porównywać całe słowo, ale jeśli uda nam się znaleźć jakiś fragment, ale trafimy na złą literkę, to zadamy sobie takie samo pytanie jak podczas przedłużania prefikso-sufiksu.

### Trikowe wyszukiwanie

Jeśli wcześniejsza koncepcja zdaje się być nieco skomplikowana, prostszym koncepcyjnie sposobem jest dokleić słowo.

Weźmy wzorzec **abaa** oraz tekst **abcdabaad**. Żeby znaleźć wzorzec w tekście, wystarczy dokleić go na samym początku, oddzielić nielegalnym dla użytkownika znakiem (np. #) i znaleźć najdłuższe prefikso-sufiksy.

Co się wtedy dzieje?

| a | b | a | a | # | a | b | c | d | a | b | a | a | d |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 0 | 0 | 1 | 1 | 0 | 1 | 2 | 0 | 0 | 1 | 2 | 3 | 4 | 0 |

Wzorzec jest tam, gdzie udało nam się znaleźć prefikso-sufiks o jego długości.

## Drzewa trie

### Wszystkie wystąpienia wzorca

:::success
Zadanie. Dane są słowa $T$ i $W$. Znajdź wszystkie wystąpienia $W$ w $T$.
:::

Wykorzystamy do tego **drzewo trie**. Zobaczmy na przykład.

Mamy zbiór słów:  
$$abcd, ab, cd, bcd, babc, baba$$

![image](https://hackmd.io/_uploads/B1aQW-oQex.png)

W tym drzewku oznaczamy dodatkowo końce słów (kwadraty).

Aby znaleźć wszystkie wystąpienia wzorca, wystarczy zbudować drzewo na wszystkich sufiksach tekstu.

Dzięki temu możemy przejść liniowo po każdym początku słowa i porównać je z wzorcem.

### Słowa spełniające warunek

:::success
Zadanie. Dana jest wartość $K$. Sprawdź, ile jest takich par $A, B$, że $A$ ^ $B \geq K$?
:::

Weźmy $K = 1010$ oraz taki zbiór liczb:

$$1010, 1011, 0101, 1101, 0011$$

Wpakujemy to teraz do drzewa trie.

![image](https://hackmd.io/_uploads/H1bIoQxQxg.png)

Ustalmy, że jeśli krawędź od ojca idzie w lewo, to wierzchołek jest $0$, a jeśli w prawo to $1$. Teraz wierzchołek będzie trzymał informację o elementach w poddrzewach.

![image](https://hackmd.io/_uploads/rJTLimeXlx.png)

Chcemy teraz sprawdzić, ile będzie par $A$ ^ $B \geq K$, porównując z $0000$.

![image](https://hackmd.io/_uploads/BJsvjmeXel.png)

Lewa strona odpada z automatu, bo wszystkie liczby zacznają się od $0$. W prawym poddrzewie mamy lepszą sytuację, bo zsumujemy poddrzewo $101\_$ oraz $11\_$.

# Wykład 14: Teoria gier

## Przykłady gier

:::info
Definicja. Nim to gra, w której ruch polega na zdejmowaniu przedmiotów ze stosu. Przegrywa ten, kto nie może wykonać ruchu.
:::

Weźmy taki stosik.

![image](https://hackmd.io/_uploads/S1km2m2mlg.png)

Aby wygrać tę grę, wystarczy zdjąć od razu wszystkie kamienie.

Teraz zmodyfikujmy nieco zasady.

:::info
Definicja. $T$ to gra, w której ruch polega na zdejmowaniu **1**, **2** lub **3** przedmiotów ze stosu. Przegrywa ten, kto nie może wykonać ruchu.
:::

Mamy teraz taką sytuację.

![image](https://hackmd.io/_uploads/ryc237hQel.png)

Aby wygrać taką grę, wystarczy utrzymywać sytuację, w której liczba kamieni jest podzielna przez 4. Na tym przykładzie, wystarczyłoby zabrać 3, bo po ruchu przeciwnika zostanie co najmniej jeden kamień ale też co najwyżej 3. A to wystarczy, byśmy mogli wykonać ostatni ruch.

Zobaczmy na przykład jeszcze jednej gry.

:::info
Definicja. $S$ to gra, w której ruch polega na zdejmowaniu **1**, **3** lub **4** przedmiotów ze stosu. Przegrywa ten, kto nie może wykonać ruchu.
:::

Dla stosu z siedmioma kamieniami.

![image](https://hackmd.io/_uploads/rJg-pX2mex.png)

Tutaj przewidzenie wygranej jest nieoczywiste. Prześledźmy, jak wygląda nasza sytuacja dla poszczególnych stosików.

| ilość kamieni | 0 | 1 |
|:-------------:|:-:|:-:|
| wynik gry     | P | W |

Dla liczby kamieni 2 możemy zagrać tylko zdejmując jeden kamień. A to doprowadza nas do stanu 1, który wiemy, że jest wygraną, ale dla przeciwnika.

| ilość kamieni | 0 | 1 | 2 |
|:-------------:|:-:|:-:|:-:|
| wynik gry     | P | W | P |

Podobnie możemy uzupełnić resztę. Jeśli w odległości $1$, $3$ lub $4$ jest przegrana, to wygrywamy.

| ilość kamieni | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 |
|:-------------:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:--:|:--:|:--:|
| wynik gry     | P | W | P | W | W | W | W | P | W | P | W  | W  | W  |

---

## Uogólnienie

:::info
Definicja. Niech $G$ będzie grą spełniającą warunki:
- gracze wykonują naprzemienne ruchy
- ruchy zależą tylko od pozycji
- jest skończona
- przegrywa ten, kto nie ma ruchu
- pełna informacja
:::

Inaczej ujmując to jest graf skończony, acykliczny i spójny, w którym zaznaczona jest wygrana/przegrana.

![image](https://hackmd.io/_uploads/rJZ_6m2Xlg.png)

Jest jeszcze jeden problem. Wygrana wygranej nie równa. Gdyby połączyć dwie gry wygrane, wcale nie musi być to wygrana.

![image](https://hackmd.io/_uploads/BynKpmnXle.png)

---

## Podobieństwo gier

:::info
Twierdzenie. Każda gra $G$ jest podobna do jakiegoś Nima.
:::

Indukcja.

$G$-przegrywająca $\rightarrow G = N_0$

W przeciwnym przypadku mamy grę, która prowadzi do jakichś stanów.

![image](https://hackmd.io/_uploads/HyCURQhXgx.png)

Stąd $G = N_m$, gdzie  $m = \text{mex}\{n_1,n_2,...,n_k\}$

$\text{mex}$ to minimum excluded, czyli wybiera najmniejszą liczbę naturalną, której nie ma w zbiorze. Na przykład:

$\text{mex}\{0,1,5,8,2\} = 3$

Dlaczego? Mamy dwa przypadki.
- $G \rightarrow N_{n_i}$, gdzie $n_i  m$, czyli przeciwnik może iść do $N_m$

Do czego jest w takim razie podobne $N_a + N_b$? Zobaczmy na przykład. Możemy uzupełnić te przykłady za pomocą $\text{mex}$.

![image](https://hackmd.io/_uploads/SJoQ0X3Qlg.png)

Patrzymy na wiersze, kolumny i wybieramy z nich $\text{mex}$. Ale okazuje się, że $N_a + N_b = N_{a XOR b}$

**Obserwacja:**
- $\text{XOR} \neq 0 \rightarrow \text{XOR} = 0$, jeśli $\text{XOR}$ stosów nie jest równy 0, to znaczy, że możemy coś zdjąć, aby wyszedł $\text{XOR}$ równy 0
- $\text{XOR} = 0 \rightarrow \text{XOR} \neq 0$, jeśli $\text{XOR}$ stosów był równy 0, to zdejmując cokolwiek będzie w następnym ruchu $\text{XOR}$ różny od 0

---

Jak w takim razie poradzić sobie z wcześniejszymi grami?
- $T_n = N_{n\%4}$
- Dwa stosy: $S_6$ oraz $S_4$. Znowu możemy wypełnić tabelę za pomocą $\text{mex}$.

| S | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 |
|---|---|---|---|---|---|---|---|---|---|---|----|----|----|
| N | 0 | 1 | 0 | 1 | 2 | 3 | 2 | 0 | 1 | 0 | 1  | 2  | 3  |

Czyli $S_6 = N_2$, $S_4 = N_2$. A stąd wystarczy korzystać z $\text{XOR}$.