[(back)](../)

# Lista 3 - Chińskie twierdzenie o resztach, Union Find
## Zad 1 - Chińskie twierdzenie o resztach (CRT)
### Cel: 
Znaleźć liczbę $A$, która spełnia $A \equiv ai (\mod pi)$ dla każdej pary $(pi, ai)$ - liczby pi są parami względnie pierwsze (pierwsze)

### Główna idea:

1. Oblicz `MOD = p1 * p2 * ... * pk`
2. Dla każdej pary $(pi, ai)$:
   - `Mi = MOD / pi`
   - `inv = inverse(Mi mod pi)` - odwrotność modularna
   - `A += ai * Mi * inv`

Wynik: `A % MOD`

### Pseudokod:
```py
MOD = Π pi
res = Σ ai * (MOD / pi) * modinv(MOD/pi, pi)
```

### Technika:
Rozszerzony algorytm Euklidesa do liczenia odwrotności modulo




## Zad 2 - Łączenie zbiorów
### Cel: 
Dla każdego wierzchołka drzewa oblicz liczbę różnych kolorów w jego poddrzewie

### Technika:
DFS post-order

Dla każdego wierzchołka tworzymy sety z kolorami, mergeując sety dzieci do setu rodzica zawsze przesypujemy mniejszy do większego, aby przyspieszyć działnie

### Pseudokod:
```py
dfs(v):
    dla każdego dziecka u:
        dfs(u)
        jeśli zbiór u jest większy niż zbiór v: zamień
        merge(zbiór u, zbiór v)
    dodaj kolor v do zbioru
```


## Zad 3 - Spójne fragmenty
### Cel:
Po każdej dodanej krawędzi:
- wypisz liczbę spójnych składowych
- rozmiar największej z nich

### Technika:
- Union-Find z rozmiarem komponentu
- `sub_size[x]` tylko dla korzenia
- Początkowo `cc = N`, potem dekrementowane przy każdym unii

### Pseudokod:
```py
uf_union(a, b):
    jeśli różne korzenie: cc--, scal mniejszy w większy
    aktualizuj rozmiar największego komponentu
```


## Zad 4 - Współczynnik różnorodności (graf dynamiczny)
### Cel: 
Po każdej nowej krawędzi wypisz `(max_vertex_number - min_vertex_number) * liczba_krawędzi` dla komponentu, który zawiera nowo dodaną krawędź

### Technika:
- Union-Find z rozszerzonymi danymi:
  - min/max wierzchołek
  - liczba krawędzi

- Obsługa cykli (czyli dodanie krawędzi do już połączonego komponentu)

### Pseudokod:
```py
uf_union(a, b):
    jeśli różne korzenie: scal dane (min, max, edges + 1)
    jeśli ten sam korzeń: zwiększ liczbę krawędzi

współczynnik = (max - min) * edges
```


## Zad 5 - Parzystość przedziału – maksymalne prawdziwe podzbiory zdań
### Cel: 
Dla każdego zdania a, b, p:
- p = 0 jeśli suma odległości od a do b parzysta
- p = 1 jeśli nieparzysta

Znajdź największy prefiks zdań, które nie są ze sobą sprzeczne

### Technika:
Union-Find z przechowywaniem parzystości względem korzenia  
Operacje z XOR (^) aby wykryć sprzeczność    
Jeśli parity[a] ^ parity[b] != p i są w tym samym zbiorze - sprzeczność  

### Pseudokod:
```py
uni(a, b, p):
    jeśli find(a) == find(b):
        return parity[a] ^ parity[b] == p
    scalaj z ustawieniem relacji XOR
```


