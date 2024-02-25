[(wróć)](../)

# Preliminaria
## Problem mnożenia liczb naturalnych
### Algorytm 1
_a_ razy dodać do siebie liczbę _b_

### Algorytm 2
Mnożenie pisemne

### Algorytm 3
Mnożenie rosyjskie - interpretujemy _a_ i _b_ jako liczby binarne, wystarczy odpowiednią potęgę _2_ (np liczby _b_) przemnożyć przez _a_ i dodać do wyniku, jednak aby uniknąć mnożenia przez _a_ przesuwamy je jedynie w prawo (bitowo, czyli mnożymy przez _2_). $a*b = \sum_{i=0}^{n} b_i 2^i a$

## Liczby Fibonacciego
### Rekurencja
```cpp
fib(n) {
    if(n <= 1) return n;
    return fib(n-1) + fib(n-2);
}
```

### Iteracja 
```cpp
fib(n) {
    if(n <= 1) return n;
    a = 0, b = 1;
    for(i = 2; i <= n; i++) {
        c = a + b;
        a = b;
        b = c;
    }
    return b;
}
```

### Potęgowanie macierzy
Korzystamy z faktu że $F_{n+1} = F_n + F_{n-1}$, więc możemy to zapisać jako $F_{n+1} = \begin{bmatrix} 1 & 1 \\ 1 & 0 \end{bmatrix} \begin{bmatrix} F_n \\ F_{n-1} \end{bmatrix}$. Wtedy $F_{n+1} = \begin{bmatrix} 1 & 1 \\ 1 & 0 \end{bmatrix}^n \begin{bmatrix} F_1 \\ F_0 \end{bmatrix}$

## Złożoność algorytmów
### Złożoność czasowa
liczba jednostek czasu potrzebna do wykonania algorytmu

### Złożoność pamięciowa
liczba jednostek pamięci potrzebna do wykonania algorytmu

### Kryterium jednorodne
koszt każdej operacji elementarnej jest jednostkowy

### Kryterium logarytmiczne
koszt każdej operacji elementarnej jest proporcjonalny do logarytmu rozmiaru danych

## Przykład: algorytm sortowania liczb
### Insert sort
w i-tej iteracji wstawiamy i-ty element na odpowiednie miejsce w posortowanej tablicy  
Czas najgorszego przypadku: $O(n^2)$  
Czas najlepszego przypadku: $O(n)$ 
 
### Select sort
w i-tej iteracji wybieramy najmniejszy element z tablicy i zamieniamy go z i-tym
Czas najgorszego przypadku: $O(n^2)$
Czas najlepszego przypadku: $O(n^2)$

## Podstawowe algorytmy i struktury danych
(w szczególności)  
* sortowanie przez scalanie
* algorytm Euklidesa (również rozszerzony)
* DFS, BFS
* znajdowanie MST - Prim, Kruskal
* Dijkstra
* Warshall-Floyd
* Ford-Fulkerson
