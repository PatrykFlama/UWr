[(back)](../)

# Lista 5 - Minimum na przedziałach (RMQ)

## Zad 1 - Sparse Table
Minimum na przedziale, bez modyfikacji tablicy

## Złożoność:
- Budowanie: O(n log n)
- Zapytania: O(1)

## Koncept:
Budujemy tablicę `ST`, gdzie `ST[k][i]` przechowuje minimum z przedziału `[i, i + 2^k - 1]`  
Odpowiedź na zapytanie minimum to `min(ST[k][L], ST[k][R - 2^k + 1])`, gdzie `k = floor(log2(R - L + 1))`  

## Pseudokod:
```py
ST[0][i] = tab[i] dla i = 0..n-1
Dla k = 1..log2(n):
    Dla i = 0..n - 2^k:
        ST[k][i] = min(ST[k-1][i], ST[k-1][i + 2^(k-1)])

Aby znaleźć min([L, R]):
    len = R - L + 1
    k = floor(log2(len))
    return min(ST[k][L], ST[k][R - 2^k + 1])
```

## Zad 2 - Divide & Conquer
Algorytm offline, bez modyfikacji tablicy

### Złożoność:
- $O(n \log n + q \log n)$

### Koncept:
Na pytania odpowiadamy offline, robimy to rekurencyjnie  
Gdy w rekurencji dostajemy przedział możemy:
- oddelegować zapytania, które są całkowicie po lewej lub prawej stronie
- odpowiedzieć na zapytania, które przecinają przedział, budując prefiksy minimum z lewej i prawej strony

### Pseudokod:
```py
Funkcja solve(L, R, queries):
    Jeśli L == R:
        Odpowiedz na każde zapytanie wartością tab[L]
    Inaczej:
        mid = (L + R) / 2
        Podziel zapytania:
            - lewostronne: R <= mid
            - prawostronne: L > mid
            - przecinające: L <= mid < R
        Rekurencyjnie solve(L, mid, lewostronne)
        Rekurencyjnie solve(mid+1, R, prawostronne)

        Dla przecinających:
            Zbuduj prefiksy minimum z lewej i prawej strony
            Dla każdego q: min(minL[q.L], minR[q.R])
```


## Zad 3 - Monotonic Queue
Offline

### Złożoność:
Przetwarzanie zapytań: $O(n + q)$

### Koncept:
Mamy tablicę oraz zapytania, posortujemy zapytania po końcu przedziału  
Chcemy stworzyć kolejkę monotoniczną (czyli strukturę w której manualnie utrzymujemy porządek):
- dodajemy element
  - na koniec (będą to elementy o dotychczas największym indeksie)
  - usuwamy wszystkie elementy z końca, które są większe od dodawanego (bo przy każdym zapytaniu, nasz element i tak by z nimi 'wygrał')
- szukamy minimum
  - dla danego lewego przedziału, binsearchujemy w kolejce ostatni element który się mieści w zapytaniu - bedzie to minimum

### Pseudokod:
```py
Posortuj zapytania po końcu przedziału r

Dla i = 0..n-1:
    Dodaj tab[i] do kolejki monotonicznej (usuwając z końca większe)
    Dla wszystkich zapytań gdzie r == i:
        Szukaj w kolejce najmniejszej wartości z indeksu >= L (np. przez binarne wyszukiwanie)
```
