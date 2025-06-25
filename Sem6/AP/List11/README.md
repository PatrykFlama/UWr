[(back)](../)

# Lista 11 - grafy


## Zad 1 - Sortowanie topologiczne
### Zadanie: 
Posortuj wierzchołki skierowanego grafu acyklicznego tak, aby dla każdej krawędzi `(u, v)`, wierzchołek u był przed v. Jeśli wiele odpowiedzi - wypisz leksykograficznie najmniejszą. Jeśli cykl - wypisz `IMPOSSIBLE`.

### Koncept:
Używamy sortowania topologicznego Kahn’a z kolejką priorytetową, aby zachować porządek leksykograficzny.
Sprawdzamy istnienie cyklu przez porównanie liczby odwiedzonych wierzchołków z N.

### Dlaczego działa:
Wierzchołki bez krawędzi wchodzących (`indegree = 0`) dodajemy do kolejki. Zdejmując wierzchołek z kolejki, usuwamy jego krawędzie i aktualizujemy indegree sąsiadów.

### Złożoność: 
O(N + M)

### Pseudokod:
```py
for each (u → v) in edges:
    indegree[v] += 1

queue = min_heap of nodes with indegree == 0

while queue not empty:
    u = queue.pop()
    result.append(u)
    for v in adj[u]:
        indegree[v] -= 1
        if indegree[v] == 0:
            queue.push(v)

if result.size != N:
    print("IMPOSSIBLE")
else:
    print(result)
```


## Zad 2 - Zbiory (z2)
### Zadanie: 
Znajdź silnie spójne składowe (SCC), tzn. takie zbiory wierzchołków, że każdy może dojść do każdego w zbiorze.

### Koncept:
Używamy algorytmu Kosaraju:

DFS w oryginalnym grafie.

DFS w transponowanym grafie w odwrotnej kolejności post-order.

### Dlaczego działa:
W pierwszym DFS zbieramy kolejność odwiedzin. W transponowanym grafie zaczynamy DFS od ostatnio odwiedzanych i każda komponenta DFS daje jedną silnie spójną składową.

### Złożoność: 
O(N + M)

### Pseudokod:
```py
DFS(u):
    mark u visited
    for v in adj[u]: if not visited[v]: DFS(v)
    post_order.push(u)

DFS_transposed(u):
    mark u visited
    component.append(u)
    for v in transposed_adj[u]: if not visited[v]: DFS_transposed(v)

for u in 1..N:
    if not visited[u]: DFS(u)

reverse(post_order)

for u in post_order:
    if not visited[u]: DFS_transposed(u), assign component number
```



## Zad 3 - Zbieranie monet
### Zadanie: 
Dla skierowanego grafu, każdy wierzchołek ma monety. Maksymalizuj sumę zebranych monet, wędrując zgodnie z krawędziami, zbierając monety tylko raz.

### Koncept:

Znajdź SCC -> superwierzchołki.

Zbuduj DAG z SCC.

Uruchom DAG DP na posortowanych wierzchołkach, maksymalizując sumę monet.

### Dlaczego działa:
Każdy cykl należy do jednej SCC i monety zbieramy tylko raz. Po kompresji grafu, mamy DAG i możemy bezpiecznie stosować DP po sortowaniu topologicznym.

### Złożoność: 
O(N + M)

### Pseudokod:
```py
1. Oblicz SCC i dla każdego SCC zsumuj monety.
2. Zbuduj DAG: SCC jako wierzchołki, krawędź między SCC jeśli istnieje oryginalna.
3. Oblicz toposort i uruchom DP:
   dp[u] = max(dp[u], dp[v] + coins[u]) dla każdej krawędzi v → u
```



## Zad 4 - Odwiedzanie krawędzi
### Zadanie: 
Znajdź cykl Eulera w grafie nieskierowanym - ścieżkę z wierzchołka 1, która przechodzi przez każdą krawędź dokładnie raz i wraca do 1.

### Koncept:
Sprawdź warunki cyklu Eulera:

Stopień każdego wierzchołka parzysty.

Graf spójny względem krawędzi.

Używamy algorytmu Hierholzera, który rekurencyjnie rozkłada cykl.

### Złożoność: 
O(M log d) z unordered_set, lub O(M) z listą.

### Pseudokod:
```py
for v in 1..N:
    if deg[v] is odd: print("IMPOSSIBLE")

DFS(u):
    while adj[u] not empty:
        v = remove_edge(u)
        DFS(v)
    path.push(u)

DFS(1)
if path.size() != M + 1: print("IMPOSSIBLE")
else: print(reverse(path))
```


## Zad 5 - Odwiedzanie krawędzi 2 
### Zadanie: 
Znajdź ścieżkę Eulera z wierzchołka 1 do N w grafie skierowanym, odwiedzając każdą krawędź dokładnie raz.

### Koncept:
Sprawdź warunki ścieżki Eulera w grafie skierowanym:

`out[1] == in[1] + 1`

`in[N] == out[N] + 1`

Dla pozostałych `in == out`

Hierholzer modyfikowany na ścieżkę, startując z 1, kończąc w N.

### Złożoność: 
O(M)

### Pseudokod:
```py
check Euler path conditions
stack = [1]
while stack not empty:
    while unvisited edge from top:
        stack.push(next node)
    else:
        path.push(stack.pop())

if path.size() != M + 1 or path[0] != N:
    print("IMPOSSIBLE")
else:
    print(reverse(path))
```


