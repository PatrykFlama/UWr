# Wzory
## Teoria liczb
### Rozszerzony Euklides
Dla danych liczb $a,b$ znajdź takie $k, l$, że $ak + bl = nwd(a,b)$.  
- $nwd(a,0) = a = a \cdot 1 + 0 \cdot 0$
- $nwd(b,a\%b)$  
Algorytm zwrócił $d=nwd(b,a\%b)=nwd(a,b)$ oraz parę $(k,l)$ taką, że $bk+(a\%b)l=d$   
$$a\%b = a - \left\lfloor\frac{a}{b}\right\rfloor b$$  
$$bk +  \left( a - \left\lfloor\frac{a}{b}\right\rfloor b \right) l = d$$   
$$bk + al - \left\lfloor\frac{a}{b}\right\rfloor bl = d$$  
$$al + b \left( k - \left\lfloor\frac{a}{b}\right\rfloor l \right) = d$$  


### Odwrotność modularna
Szukamy $aa^{-1} \equiv 1 \mod m$, 
- Euklidesem $ak + ml = nwd(a,m) = 1$, więc $k \equiv a^{-1} \mod m$  
- Małe tw Fermata (zał że $m$, $p$ pierwsze): 
  - $a^{p-1} \equiv 1 \mod p$
  - $a \cdot a^{m-2} \equiv 1 \mod m$


### Funkcja Eulera

Jeśli $p$ jest pierwsza, to $\phi(p)=p-1$.  
Uzasadnienie: wszystkie liczby między $1$ a $p-1$ włącznie są względnie pierwsze z $p$.

Jeśli $p$ jest pierwsza, to $\phi(p^k)=p^k-p^{k-1}=p^k(1-\frac{1}{p})$.  
Uzasadnienie: między $1$ a $p^k$ co $p$-ta liczba dzieli się przez $p$, zatem pozostałych liczb jest $p^k - \frac{p^k}{p} = p^k-p^{k-1}$.

Jeśli $a,b$ są względnie pierwsze, to $\phi(ab)=\phi(a)\phi(b)$.  

### Chińskie twierdzenie o resztach
Niech $M = p_1 \cdot p_2 \cdot \ldots \cdot p_N$.  
Naszym celem jest znalezienie takich wartości $w_1, \ldots, w_N$, że dla każdego $1 \leq i \leq N$ będą spełnione własności:

- $w_i \bmod p_i = a_i$  
- $w_i \bmod p_j = 0$ dla $i \neq j$

Wtedy rozwiązaniem będzie po prostu:
$$
x = (w_1 + w_2 + \ldots + w_N) \bmod M
$$
co łatwo zweryfikować.

Aby spełnić drugą własność, każdy element $w_i$ możemy pomnożyć przez $m_i = \frac{M}{p_i}$ — wtedy $w_i$ będzie dawał wynik $0$ modulo wszystkie $p_j$ dla $j \neq i$.

Zauważmy, że jeśli pomnożymy teraz $m_i$ przez $r_i = m_i^{-1} \bmod p_i$ (odwrotność modularna), uzyskamy wartość $1$ modulo $p_i$.

Zatem ten iloczyn wystarczy pomnożyć przez $a_i$.

Podsumowując, aby obliczyć wynik, musimy wyznaczyć kolejno:
- $M = p_1 \cdot p_2 \cdot \ldots \cdot p_N$
- $m_i = \frac{M}{p_i}$
- $r_i = m_i^{-1} \bmod p_i$
- $w_i = m_i \cdot r_i \cdot a_i \bmod M$
- $x = (w_1 + w_2 + \ldots + w_N) \bmod M$


### Dwumian Newtona
$${n \choose k} = {n-1 \choose k} + {n-1 \choose k-1}$$



## Geometria
### Iloczyn wektorowy
Iloczyn wektorowy dwóch wektorów $\vec{a} = (x_1, y_1)$ i $\vec{b} = (x_2, y_2)$ w przestrzeni 2D jest zdefiniowany jako:  
$$\vec{a} \times \vec{b} = x_1 y_2 - x_2 y_1$$

Dla wektorów $\vec{a}$ i $\vec{b}$, iloczyn wektorowy daje informuje, który ma większy kąt względem osi OX:
- Jeśli $\vec{a} \times \vec{b} > 0$, to $\vec{a}$ jest "na lewo" od $\vec{b}$
- Jeśli $\vec{a} \times \vec{b} < 0$, to $\vec{a}$ jest "na prawo" od $\vec{b}$
- Jeśli $\vec{a} \times \vec{b} = 0$, to $\vec{a}$ i $\vec{b}$ są współliniowe (leżą na tej samej prostej)

Dla danych 2 punktów połowa iloczynu wektorowego daje pole trójkąta utworzonego przez te punkty i punkt $(0, 0)$:
$$\text{Pole} = \frac{1}{2} | \vec{a} \times \vec{b} |$$

Dodatkowo korzystając z tych zależności można wywnioskować, że dla 3 punktów, iloczyn wektorów łączących te punkty określa, czy punkt trzeci leży na prostej wyznaczonej przez dwa pozostałe:
- Jeśli iloczyn wektorowy jest równy zero, to punkt leży na prostej
- Jeśli jest dodatni, to punkt leży po lewej stronie prostej
- Jeśli jest ujemny, to punkt leży po prawej stronie prostej


### Pole wielokąta
Wzór polega na policzeniu pola każdego trójkąta, który dodatkowo daje wartość ujemną dla *złych* trójkątów (czyli takich, które mają kąty malejące).

$$
\text{Pole} = \frac{1}{2} \left| \sum_{i=0}^{n-1} (x_i y_{i+1} - x_{i+1} y_i) \right|
$$

## Odległość od prostej
Dla $p_1, p_2$ na prostej i $p_q$ jako punktu:


$$
d = \frac{\,| (x_2 - x_1)(y_1 - y_q) - (x_1 - x_q)(y_2 - y_1) |\,}{\sqrt{(x_2 - x_1)^2 + (y_2 - y_1)^2}}
$$


> Jest to iloraz wartości bezwzględnej iloczynu wektorowego $\vec{p_1 p_2} \times \vec{p_1 p_q}$ i długości wektora $\vec{p_1 p_2}$.

---


# Zadania
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




