[(back)](../)

# Lista 10 - grafy z wagami
## Zad 1 - Najkrótsze ścieżki
### Koncept
Klasyczny algorytm Dijkstry (kolejka priorytetowa) pozwala obliczyć najkrótsze odległości z jednego źródła (tu: 1) do wszystkich innych w grafie o nieujemnych wagach.

### Złożoność
Czasowa: $O((N+M) \log N)$  
Pamięciowa: $O(N+M)$

### Pseudokod
```py
dist[1..N] = INF
dist[1] = 0
queue.push((0, 1))

while queue not empty:
    (d, u) = queue.pop()
    for (v, c) in adj[u]:
        if dist[u] + c < dist[v]:
            dist[v] = dist[u] + c
            queue.push((dist[v], v))
```


## Zad 2 - Kupon rabatowy
### Koncept
Dla każdego wierzchołka rozważ dwa stany:
- bez użycia kuponu (oryginalna krawędź)
- po użyciu kuponu (jedna krawędź o połowie kosztu)

Tworzymy rozszerzony graf 2N i puszczamy Dijkstrę.

### Złożoność
Czasowa: $O((2N + 2M) \log {(2N)})$

### Pseudokod
```py
dla każdej krawędzi u → v o koszcie w:
    dodaj: u → v wagą w
    dodaj: u → v' wagą w/2  (przejście z "nieużyty" → "użyty")
    dodaj: u' → v' wagą w

dist[1] = 0, reszta = ∞
PQ ← {(0, 1)}
while PQ not empty:
    (d, v) ← pop min
    for (v → u, w):
        if dist[u] > dist[v] + w:
            dist[u] = dist[v] + w
            push (dist[u], u)
```


## Zad 3 - Najkrótsze ścieżki 2
### Koncept
Używamy algorytmu Floyd'a–Warshall'a, bo liczba wierzchołków jest mała ($N \leq 500$). Odpowiadamy na 100k zapytań w 𝑂(1) po precomputingu

### Złożoność
Czasowa: $O(N^3 + Q)$  
Pamięciowa: $O(N^2)$


### Pseudokod
```py
dist[i][j] = ∞
for każda krawędź u, v, w:
    dist[u][v] = dist[v][u] = min(dist[u][v], w)
for i:
    dist[i][i] = 0

// Floyd-Warshall
for k in 1..N:
    for i in 1..N:
        for j in 1..N:
            dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j])

for każde zapytanie u, v:
    if dist[u][v] == ∞: wypisz -1
    else wypisz dist[u][v]
```


## Zad 4 - Ujemny cykl
### Koncept
Używamy Bellmana-Forda - po N relaksacjach, jeśli dalej można zrelaksować, mamy ujemny cykl. Odtwarzamy go przez śledzenie parentów

### Złożoność
Czasowa: $O(NM)$

Pamięciowa: $O(N+M)$


### Pseudokod
```py
dist[i] = 0 dla każdego i, parent[i] = -1
for i = 1 to N:
    for każda krawędź u → v o wadze w:
        if dist[v] > dist[u] + w:
            dist[v] = dist[u] + w
            parent[v] = u
            x = v

if x == -1:
    wypisz NO
else:
    przejdź n razy przez parent[x] → znajdź początek cyklu
    idź po parentach i zapisz cykl, aż znowu trafisz na początek
    wypisz YES i cykl
```


## Zad 5 - Najdłuższa ścieżka
### Koncept
Zamieniamy problem najdłuższej ścieżki w odwrócenie znaku wag, a potem klasyczny Bellman-Ford. Jeśli można zrelaksować po N krokach - istnieje cykl ulemy (ale wagi są odwrócone więc w oryginalnym grafie dodatni).  
Musimy się jeszcze upewnić, że wierzchołki/cykl z którego korzystamy jest osiągalny z 1 i da się z niego dojść do N. Wystarczy policzyć tablicę odwiedzonych na odwrotnym grafie. 






