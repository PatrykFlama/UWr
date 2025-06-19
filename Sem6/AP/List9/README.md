[(back)](../)

# Lista 9 - operacje na drzewach
## Zad 1 - Przodek
Znajdź `k`-tego przodka wierzchołka `v` w drzewie

### Koncept:
Zastosuj Binary Lifting - dla każdego wierzchołka zapamiętaj jego przodków na poziomach $2^i$. Aby znaleźć przodka `k` poziomów wyżej, przeskakuj po bitach `k` (jeśli `i`-ty bit ustawiony, idź $2^i$ w górę).

### Złożoność:
Preprocessing: O(N log N)  
Każde zapytanie: O(log N)  


### Pseudokod:
```py
build():
  jump[v][0] = parent of v
  for j = 1 to logN:
    jump[v][j] = jump[ jump[v][j-1] ][j-1]

query(v, k):
  for i = 0 to logN:
    if k & (1 << i):
      v = jump[v][i]
      if v == -1: return -1
  return v
```


## Zad 2 - Najniższy wspólny przodek (LCA)
Znajdź LCA (Lowest Common Ancestor) dwóch wierzchołków.

### Koncept:
Binary lifting, podobnie jak wcześniej w z1. Wyrównujemy poziomy, potem równocześnie 'skaczemy' w górę, aż znajdziemy wspólnego przodka.

### Złożoność:
Preprocessing: O(N log N)  
Zapytanie: O(log N)  

### Pseudokod:
```py
LCA(u, v):
  if depth[u] < depth[v]: swap(u, v)
  u = jump_up(u, depth[u] - depth[v])
  if u == v: return u
  for i = logN downto 0:
    if jump[u][i] != jump[v][i]:
      u = jump[u][i]
      v = jump[v][i]
  return jump[u][0]
```


## Zad 3 - Zliczanie ścieżek
Podano *M* ścieżek. Policz, przez ile ścieżek przechodzi każdy wierzchołek.

### Koncept:
Użyj Binary Lifting do wyznaczenia `LCA(u, v)`, następnie:
- Zwiększ *+1* w `u` i `v`
- Zmniejsz *-1* w `LCA` oraz jego rodzicu
- Na koniec wykonaj DFS, sumując wartości w dół.

### Złożoność:
Dodanie M ścieżek: O(M log N)  
DFS sumujący wyniki: O(N)  

### Pseudokod:
```py
for each path(u, v):
  count[u]++
  count[v]++
  count[LCA(u,v)]--
  count[parent[LCA(u,v)]]--

dfs(v, parent):
  for u in children[v]:
    dfs(u, v)
    count[v] += count[u]
```


## Zad 4 - Odległości na drzewie
Policz odległość między dwoma wierzchołkami

### Koncept:
Odwiedź wszystkie wierzchołki DFS-em tworząc Euler Tour oraz tablicę głębokości. Użyj RMQ (segment tree) do szybkiego znajdowania `LCA(u, v)`, a odległość to:  
`depth[u] + depth[v] - 2 * depth[lca]`.

### Złożoność:
Preprocessing: O(N)  
Zapytania: O(log N)   

### Pseudokod:
```py
dfs(v, d):  // d = depth
  depth[v] = d
  euler.push(v)
  for u in adj[v]:
    if u != parent:
      dfs(u, d+1)
      euler.push(v)

query(u, v):
  lca = RMQ(first[u], first[v])
  return depth[u] + depth[v] - 2 * depth[lca]
```

## Zad 5 - Sumy w poddrzewach
Zmiana wartości w wierzchołku i sumowanie wartości w jego poddrzewie

### Koncept:
Zrób Euler Tour - przekształć każde poddrzewo w spójny przedział. Następnie użyj segment tree do sumowania i aktualizacji wartości w $O(\log N)$

### Złożoność:
Preprocessing: $O(N)$  
Zapytanie/zmiana: $O(\log N)$  

### Pseudokod:
```py
dfs(v):
  tin[v] = time++
  for u in adj[v]: dfs(u)
  tout[v] = time

sum_subtree(v):
  return segment_tree.query(tin[v], tout[v])

update(v, val):
  segment_tree.update(tin[v], val)
```

## Zad 6 - Sumy na ścieżkach
Zmiana wartości w wierzchołku i suma na ścieżce do korzenia

### Koncept:
Zrób Euler Tour, ale dodaj `+val` przy wejściu i `-val` przy wyjściu z wierzchołka. Wartość ścieżki z `v` do korzenia to suma prefiksowa do momentu wejścia do `v`.

### Złożoność:
Preprocessing: $O(N)$  
Aktualizacja/zapytanie: $O(\log N)$  

### Pseudokod:
```py
dfs(v):
  in[v] = time++
  for u in adj[v]: dfs(u)
  out[v] = time++

build segment_tree over:
  seg[in[v]] = val
  seg[out[v]] = -val

update(v, x):
  seg.update(in[v], x)
  seg.update(out[v], -x)

query(v):
  return seg.query(0, in[v])
```


## Zad 7 - Wielkanocna Rodzina Kurczaków
Podaj najwyższego wspólnego przodka w spójnym ukorzenionym drzewie

### Koncept:
1

### Złożoność:
1

### Pseudokod:
```py
for each query:
  print(1)
```

