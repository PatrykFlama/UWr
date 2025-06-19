[(back)](../)

# Lista 2 - Wprowadzenie, teoria liczb
## Zad 1 - Rozszerzony Algorytm Euklidesa
### Opis:
Dla każdej pary liczb $(a, b)$ znajdź liczby całkowite $k, l$ oraz $d = \gcd(a, b)$ takie, że $a * k + b * l = d$

### Technika:
Rekurencyjny rozszerzony algorytm Euklidesa  
Oprócz gcd, wyznacza także współczynniki dla liczb $(a, b)$



### Pseudokod:
```py
def extended_gcd(a, b):
    if b == 0:
        return (1, 0, a)

    x1, y1, d = extended_gcd(b, a % b)
    x, y = y1, x1 - (a // b) * y1

    return (x, y, d)
```


## Zad 2 - Dwumian Newtona 
### Opis:
Oblicz wartość $C(a, b) = a! / (b! * (a - b)!) \mod 1e9+7$

### Technika:
Precomputing silni (factorial) oraz ich odwrotności (inv_factorial) do $1e6$  
Dzielimy przez $b!$ i $(a−b)!$ za pomocą odwrotności modularnej  

Metody odwrotności modularnej:
- Małe twierdzenie Fermata: $inv(x) = x^{(\text{mod}-2)} \% \text{mod}$
- Rozszerzony algorytm Euklidesa

### Pseudokod:

```py
precompute factorial[0..MAX]
precompute inv_factorial[0..MAX] using pow_mod(fact[i], MOD-2)

def C(a, b):
    if b < 0 or b > a: return 0
    return fact[a] * inv_fact[b] % MOD * inv_fact[a - b] % MOD
```


## Zad 3 - Zlicz dzielniki liczby
### Opis:
Dla liczby $x$, znajdź ile liczb $d$ spełnia $x \% d == 0$

### Technika:
Preprocessing tablicy wyników dla wszystkich liczb do $10^6$  
Dla każdej liczby $i$ od 1 do $L$ (gdzie $L = 10^6+2$), przechodzimy po wszystkich wielokrotnościach $j$ liczby $i$ i zwiększamy licznik dzielników $res[j]$ o 1. Dzięki temu dla każdej liczby $a$ w zakresie $[1, L)$ w $res[a]$ znajduje się liczba jej dzielników.

### Pseudokod:

```py
def preprocess():
    for i in range(1, L):
        for j in range (i, L, i):
            res[j]++

res[X]  # liczba dzielników liczby X
```

### Uwagi:
Dzięki preprocesowaniu możemy odpowiadać na zapytania o liczbę dzielników w czasie stałym $O(1)$, a całe przygotowanie tablicy zajmuje $O(L \log L)$.



## Zad 4 - Funkcja Eulera
### Opis:
Dla liczby x, wyznacz liczbę mniejszych od niej liczb względnie pierwszych z x  

$$\phi(x) = x * \Pi (1 - 1/p) \qquad \forall p \quad x | p$$


### Technika:
Preprocessing tablicy wartości funkcji Eulera dla wszystkich liczb do $10^6$ za pomocą zmodyfikowanego sita Eratostenesa  
Dla każdej liczby $i$ od 2 do $L$ (gdzie $L = 10^6+2$), jeśli $i$ jest pierwsza ($\phi[i] == i$), przechodzimy po wszystkich jej wielokrotnościach $j$ i aktualizujemy $\phi[j]$ według wzoru: $\phi[j] = \phi[j] * (i-1) / i$  

### Pseudokod:

```py
def preprocess():
    for i in range(0, L): phi[i] = i

    for i in range(2, L):
        if phi[i] == i {
            for j in range (i, L, i):
                phi[j] = phi[j] * (i - 1) / i;
```

### Uwagi:
Dzięki preprocesowaniu możemy odpowiadać na zapytania o wartość funkcji Eulera w czasie stałym $O(1)$, a całe przygotowanie tablicy zajmuje $O(L \log \log L)$  



