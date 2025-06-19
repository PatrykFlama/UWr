[(back)](../)

# Lista 7 - drzewa przedziałowe
## Zad 1 - Zliczanie inwersji
Zliczamy liczbę par `(i, j)` w tablicy, dla których `i < j` oraz `A[i] > A[j]`

### Koncept:
Zliczanie inwersji można wykonać efektywnie używając drzewa przedziałowego   Przechodzimy przez tablicę od lewej do prawej, a dla każdego `A[i]` zliczamy, ile już widzieliśmy większych wartości, czyli `query(A[i]+1, MAX)`.

Każde zapytanie `query(A[i]+1, MAX)` odpowiada liczbie liczb większych niż A[i], które pojawiły się wcześniej w tablicy — czyli inwersji kończących się na i

### Złożoność czasowa:
`O(N log MAX)`

### Pseudokod:
```py
BIT bit(MAX+1)
res = 0
for i in 0..n-1:
    res += bit.query(a[i]+1, MAX)
    bit.update(a[i], 1)
print res
```


## Zad 2 - Zmiana na przedziale
Obsługujemy dwa typy zapytań:
- `1 x y v`: dodaj v do przedziału `[x,y]`
- `2 k`: wypisz wartość na pozycji `k`

### Koncept:
Używamy leniwego drzewa przedziałowego ("lazy segment tree"). Zmiana na przedziale jest przechowywana tymczasowo w tablicy lazy, a aktualizacje są odkładane do momentu potrzeby (tzw. "push").  


### Złożoność czasowa:
$O(\log N)$ na operację

### Pseudokod:
```py
LazySegTree st(a)
for zapytanie in zapytania:
    if type == 1:
        st.update(x, y, v)
    else:
        print st.query(k, k)
```


## Zad 3 - Sumy prefiksowe na przedziałach
Dwa typy zapytań:
- `1 k v`: ustaw `a[k] = v`
- `2 x y`: znajdź największą sumę prefiksu w podciągu a`[x..y]`

### Koncept:
Każdy węzeł w drzewie przedziałowym przechowuje:  
- sumę całego przedziału
- największy prefiks

Dzięki temu możemy połączyć dane dwóch dzieci w czasie stałym

### Złożoność czasowa:
$O(\log N)$ na zapytanie i aktualizację

### Pseudokod:
```py
Node = (sum, max_pref_sum)
merge(l, r) = (l.sum + r.sum, max(l.max_pref, l.sum + r.max_pref))
st = SegmentTree(a)
for zapytanie:
    if typ == 1:
        st.update(k, (v, max(0, v)))
    else:
        print st.query(x, y).max_pref
```


## Zad 4 - Przedział o największej sumie
Po każdej zmianie wartości jednego elementu mamy wypisać maksymalną sumę spójnego podciągu

### Koncept:
Segment tree z 4 informacjami w każdym węźle:
- suma
- maksymalna suma prefiksu
- maksymalna suma sufiksu
- maksymalna suma w całym przedziale

Dzięki temu można złączyć dwa przedziały i wyznaczyć maksimum w czasie $O(1)$  

### Złożoność czasowa:
$O(\log N)$ na modyfikację i zapytanie.

### Pseudokod:
```py
Node = (sum, pref_sum, suff_sum, max_sum)
merge(l, r) = (
    l.sum + r.sum,
    max(l.pref_sum, l.sum + r.pref_sum),
    max(r.suff_sum, r.sum + l.suff_sum),
    max(l.max_sum, r.max_sum, l.suff_sum + r.pref_sum)
)
st = SegmentTree(a)
for (k, x):
    st.update(k, Node(x, max(0,x), max(0,x), max(0,x)))
    print st.query(0, N-1).max_sum
```

