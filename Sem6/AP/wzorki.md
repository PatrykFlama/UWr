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







[(back)](../)

# Lista 12 - geometria

## Zad 1 - Lokalizacja punktu

**Opis:**  
Dla trzech punktów $p_1$, $p_2$, $p_3$ określ, czy $p_3$ leży na prostej przez $p_1 \rightarrow p_2$, a jeśli nie - to czy jest po lewej, czy po prawej stronie tej prostej.

**Koncept:**  
Użycie iloczynu wektorowego:  
Niech $\vec{a} = p_2 - p_1$, $\vec{b} = p_3 - p_1$.  
Wtedy:
- $\vec{a} \times \vec{b} = 0$ &rarr; **TOUCH**
- $\vec{a} \times \vec{b} > 0$ &rarr; **LEFT**
- $\vec{a} \times \vec{b} < 0$ &rarr; **RIGHT**

**Dlaczego działa:**  
Wektorowy iloczyn dwóch wektorów w 2D wskazuje orientację: dodatni dla lewego skrętu, ujemny dla prawego, zero - współliniowe.

**Złożoność:**  
$O(T)$

**Pseudokod:**
```py
for each test (x1, y1, x2, y2, x3, y3):
    cross = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)
    if cross == 0:
        print("TOUCH")
    elif cross > 0:
        print("LEFT")
    else:
        print("RIGHT")
```



## Pole wielokąta (z2)

**Opis:**  
Policz pole nieprzecinającego się wielokąta z zadanymi wierzchołkami.

**Koncept:**  
Użycie wzoru Shoelace (sznurowadła):

$$
\text{Pole} = \frac{1}{2} \left| \sum_{i=0}^{n-1} (x_i y_{i+1} - x_{i+1} y_i) \right|
$$

**Dlaczego działa:**  
To standardowy wzór wywodzący się z całki oznaczonej po konturze (twierdzenie Greena) dla wielokąta.

**Złożoność:**  
$O(N)$

**Pseudokod:**
```py
area = 0
for i in 0 to N-1:
    x1, y1 = points[i]
    x2, y2 = points[(i+1)%N]
    area += (x1 * y2 - x2 * y1)
print(abs(area) / 2.0)
```

---

## Odległość od odcinka (z3)

**Opis:**  
Dla punktu $q$ oblicz odległość do prostej przechodzącej przez $p_1$, $p_2$.

**Koncept:**  
Skorzystaj ze wzoru na odległość punktu od prostej:

$$
d = \frac{\,| (x_2 - x_1)(y_1 - y_q) - (x_1 - x_q)(y_2 - y_1) |\,}{\sqrt{(x_2 - x_1)^2 + (y_2 - y_1)^2}}
$$

**Dlaczego działa:**  
Wynika bezpośrednio z rzutowania wektora $\vec{p_1q}$ na prostopadły do $\vec{p_1p_2}$.

**Złożoność:**  
$O(Q)$

**Pseudokod:**
```py
for each query:
    cross = abs((x2 - x1)*(y1 - yq) - (x1 - xq)*(y2 - y1))
    length = sqrt((x2 - x1)**2 + (y2 - y1)**2)
    print(cross / length)
```

---

## Widoczne punkty (z4)

**Opis:**  
Policz, ile punktów w górnej półpłaszczyźnie widać z punktu $q$ leżącego na osi OX.

**Koncept:**  
Zamień każdy punkt $p = (x, y)$ na parę $\text{slope} = \frac{y}{x - x_q}$.  
Sortuj względem tej "nachylenia", zachowując największe $y$ dla każdej nachylenia.

**Jak działa:**  
Punkt zasłania inny, jeśli ma ten sam kierunek, ale niższą wysokość (mniejszy $y$).

**Złożoność:**  
$O(N \log N)$

**Pseudokod:**
```py
for each point (x, y):
    dx = x - xq
    slope = y / dx
    store (slope, y)
sort all points by slope
for each group of equal slope:
    keep only max y
count total unique slopes
```

---

## Otoczka wypukła (z5)

**Opis:**  
Znajdź punkty leżące na otoczce wypukłej zbioru punktów.

**Koncept:**  
Użycie algorytmu Andrew's monotone chain:

- Posortuj punkty leksykograficznie.
- Zbuduj dolną i górną otoczkę, odrzucając punkty zawracające (prawe skręty).

**Dlaczego działa:**  
Każdy dodany punkt tworzy wypukłą granicę, jeśli nie zaburza rotacji w lewo.

**Złożoność:**  
$O(N \log N)$

**Pseudokod:**
```py
sort points lexicographically
for pass in [forward, reverse]:
    for p in points:
        while last two and right turn:
            pop last
        add p
remove duplicate end
```
