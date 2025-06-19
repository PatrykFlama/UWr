[(back)](../)

# Lista 6 - Programowanie dynamiczne na drzewach i maskach bitowych

## Zad 1 - Średnica drzewa
Policz średnicę drzewa

### Koncept (BFS):
Średnica drzewa to najdłuższa ścieżka między dwoma wierzchołkami  

Wystarczy wykonać dwa BFS-y:
- pierwszy z dowolnego wierzchołka – znajdzie najdalszy wierzchołek v1
- drugi z v1 – najdalszy wierzchołek od niego to koniec średnicy, a dystans to długość

### Koncept (DP)
Alternatywnie, aby skorzystać z programowania dynamicznego, stworzymy tabelę liczącą największą głębokość do liści, którą policzymy np DFS - wynik śerdnicy to maksymalna głębokość z dwóch największych dzieci  

### Pseudokod:
```py
find_furthest(v):
    BFS od v -> zapamiętaj wierzchołek o największym dystansie

main:
    v1 = find_furthest(1)
    v2 = find_furthest(v1)
    wypisz odległość do v2
```


## Zad 2 - Największe skojarzenie na drzewie 
Znajdź rozmiar największego skojarzenia w drzewie

### Koncept:
Dla każdego wierzchołka v trzymamy:
- dp[v][0] – najlepsze skojarzenie w poddrzewie v, jeśli nie bierzemy krawędzi wychodzącej z v
- dp[v][1] – najlepsze skojarzenie, jeśli bierzemy jedną krawędź z v do dziecka u

aktualizacje:
- dp[v][0] = suma maksymalnych skojarzeń z wszystkich dzieci u (czyli max(dp[u][0], dp[u][1]))
- dp[v][1] = maksymalna wartość z (1 + dp[u][0] + dp[v][0] - max(dp[u][0], dp[u][1])) dla wszystkich dzieci u
  - 1 oznacza, że bierzemy krawędź z v do u
  - dp[u][0] - to bierzemy najlepsze skojarzenie dziecka, w którym dziecko nie jest skojarzone
  - dp[v][0] - max(dp[u][0], dp[u][1]) - bierzemy najlepsze skojarzenie z v, ale odejmujemy to, które już wzięliśmy z u (aby nie liczyć krawędzi dwa razy)

### Pseudokod:
```py
dfs(v, parent):
    dp[v][0] = suma max(dp[u][0], dp[u][1]) po wszystkich dzieciach u
    dp[v][1] = max(1 + dp[u][0] + dp[v][0] - max(dp[u][0], dp[u][1])) po wszystkich dzieciach u
```

## Zad 3 - Najlepszy przydział
Mamy N pracowników i N stanowisk. Każdy pracownik może pracować tylko na jednym stanowisku, a każde stanowisko musi być zajęte

### Koncept:
Używamy bitmaskowego dynamicznego programowania, gdzie:

dp[mask] to maksymalny zysk przy przypisaniu stanowisk oznaczonych przez maskę (1 = stanowisko już zajęte)

Przechodzimy po pracownikach (indeks i), zakładając że przypisujemy ich do wolnych stanowisk

### Pseudokod:
```py
dp[0] = 0
for mask from 0 to (1<<n)-1:
    i = liczba jedynek w masce (czyli który pracownik)
    for pos in 0..n-1:
        if pos nie w masce:
            dp[mask | (1<<pos)] = max(dp[mask | (1<<pos)], dp[mask] + tab[i][pos])
```


## Z4 - Ścieżki Hamiltona
Policz ile jest ścieżek Hamiltona z 1 do N  

### Koncept:
dp[v][mask] – ile ścieżek kończących się w v, odwiedzających wierzchołki z mask

Zaczynamy od dp[0][1] = 1

### Pseudokod:
```py
dp[0][1] = 1
for mask in 1..(1<<n)-1:
    for v in 0..n-1:
        if v ∉ mask: continue
        for u ∈ graph[v]:
            if u ∈ mask: continue
            dp[u][mask | (1<<u)] += dp[v][mask]

wypisz dp[n-1][(1<<n)-1]
```

