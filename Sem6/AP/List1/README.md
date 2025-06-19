[(back)](../)

# Lista 1

## Zad 1 - Największy Wspólny Dzielnik
### Opis:
Obliczenie największego wspólnego dzielnika dwóch dużych liczb.

### Metoda rozw:
Algorytm Euklidesa (rekurencyjny lub iteracyjny)  
Działa w czasie $\log(\min(A, B))$  

### Pseudokod
```py
def gcd(a, b):
    while b != 0:
        a, b = b, a % b
    return a
```


## Zad 2 - Szybkie potęgowanie
### Opis:
Obliczenie $A^N mod 1e9$ dla bardzo dużego wykładnika $N$

### Technika:
Szybkie potęgowanie polega na dzieleniu wykładnika na pół

### Pseudokod
```py
def fast_pow(a, n, mod):
    result = 1
    while n > 0:
        if n % 2 == 1:
            result = (result * a) % mod
        a = (a * a) % mod
        n = n // 2
    return result
```


## Zad 3 - Wyszukiwanie binarne
### Opis:
Dla każdej liczby zapytania - sprawdzenie, czy jest w posortowanej tablicy

### Technika:
Klasyczne wyszukiwanie binarne $O(\log N)$ na zapytanie

### Pseudokod:

```py
def binary_search(array, x):
    l, r = 0, len(array) - 1
    while l <= r:
        mid = (l + r) // 2
        if array[mid] == x:
            return mid + 1  # numeracja 1-indeksowana
        elif array[mid] < x:
            l = mid + 1
        else:
            r = mid - 1
    return -1
```


## Zad 4 - Sortowanie
### Opis:
Posortowanie dużej tablicy liczb.

### Technika:  
np quicksort w $O(n \log n)$


## Zad 5 - Hetmany
### Opis:
Znalezienie liczby wszystkich poprawnych rozmieszczeń $N$ hetmanów na szachownicy $N*N$

### Technika:
Rekurencyjne przeszukiwanie z powrotami (backtracking)  
Sprawdzanie konfliktów kolumn i przekątnych  

### Pseudokod:
```py
def solve(row):
    if row == N:
        count += 1
        return
    for col in 0..N-1:
        if col, diag1, diag2 free:
            mark used
            solve(row + 1)
            unmark used
```


## Zad 6 - Rozmiary poddrzew
### Opis:
Dla każdego wierzchołka drzewa oblicz liczbę wierzchołków w jego poddrzewie

### Technika:
DFS od korzenia  
Zwracamy rozmiar poddrzewa - suma rozmiarów dzieci + 1  

### Pseudokod:
```py
function dfs(u):
    size[u] = 1
    for v in children[u]:
        dfs(v)
        size[u] += size[v]
```


## Zad 7 - Budowanie dróg
### Opis:
Dodanie minimalnej liczby krawędzi, by graf stał się spójny

### Technika:
Policz liczbę spójnych składowych - potrzeba dodać (komponenty - 1) krawędzi  

### Pseudokod:

```py
for all nodes:
    if not visited:
        dfs(node)
        components += 1
return components - 1
```


## Zad 8 - Najkrótsza ścieżka
### Opis:
Wyznaczenie najkrótszych odległości od wierzchołka 1 w grafie nieskierowanym

### Technika:
Przeszukiwanie wszerz (BFS) – działa w czasie liniowym  

### Pseudokod:
```py
queue.push(1)
dist[1] = 0
while queue not empty:
    u = queue.pop()
    for v in adj[u]:
        if dist[v] == -1:
            dist[v] = dist[u] + 1
            queue.push(v)
```


