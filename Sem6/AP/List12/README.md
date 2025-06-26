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


> Wzór na iloczyn wektorowy $x \times y$ w 2D to:
> $$x \times y = x_1 y_2 - x_2 y_1$$



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
Użycie wzoru na pole trójkąta, względem punktu $(0, 0)$:

$$
\text{Pole} = \frac{1}{2} \left| \sum_{i=0}^{n-1} (x_i y_{i+1} - x_{i+1} y_i) \right|
$$

**Dlaczego działa:**  
Liczymy pole każdego trójkąta składającego się na wielokąt. Dodatkowo wzór daje wartość ujemną gdy kąty są malejące, więc *złe* pola się odejmują.

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


> Jest to iloraz wartości bezwzględnej iloczynu wektorowego $\vec{p_1 p_2} \times \vec{p_1 p_q}$ i długości wektora $\vec{p_1 p_2}$.

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
Aby poprawnie rozpoznać, które punkty są widoczne, należy posortować je **kątowo** względem punktu $q$. Kątową kolejność można uzyskać, porównując wektory $(x - x_q, y)$ wychodzące z $q$ do każdego punktu i sortując je według kąta względem osi OX. Do porównania kierunków wektorów używamy iloczynu wektorowego (cross product):  
- Dla dwóch punktów $p_i$, $p_j$ porównujemy $\vec{q p_i} \times \vec{q p_j}$.  
- Jeśli iloczyn jest dodatni, $p_i$ jest "na lewo" od $p_j$ (ma mniejszy kąt).
- Jeśli iloczyn jest zerowy, punkty są współliniowe (leżą na tej samej prostej z $q$).

Po posortowaniu iterujemy po punktach i dla każdego kierunku (kąta) wybieramy tylko jeden punkt (np. najbliższy), bo pozostałe są zasłonięte.

**Dlaczego działa:**  
Sortowanie kątowe zapewnia, że punkty na tej samej prostej z $q$ sąsiadują w posortowanej liście, a iloczyn wektorowy pozwala jednoznacznie określić kolejność kątową.

**Złożoność:**  
$O(N \log N)$

**Pseudokod:**
```py
for each point (x, y):
    dx = x - xq
    dy = y
    store (dx, dy)
    
sort all stored points by angle using cross product:
    for (dx1, dy1), (dx2, dy2):
        compare (dx1, dy1) * (dx2, dy2)

iterate over sorted points:
    if current point is not collinear with previous (cross != 0):
        count visible point
```

---

## Otoczka wypukła (z5)

**Opis:**  
Znajdź punkty leżące na otoczce wypukłej zbioru punktów.

Użycie algorytmu Andrew's monotone chain:

- Najpierw posortuj wszystkie punkty leksykograficznie (najpierw po $x$, potem po $y$).
- Następnie przejdź po punktach, budując dolną otoczkę: dla każdego nowego punktu sprawdzaj, czy ostatnie dwa punkty na otoczce wraz z nowym punktem tworzą prawy skręt (iloczyn wektorowy $\leq 0$). Jeśli tak, usuń ostatni punkt z otoczki. Powtarzaj, aż nie będzie prawego skrętu.
- Analogicznie zbuduj górną otoczkę, przechodząc po punktach w odwrotnej kolejności.
- Połącz dolną i górną otoczkę (bez powtarzania punktów końcowych).

**Dlaczego działa:**  
Algorytm zachowuje tylko te punkty, które utrzymują wypukłość otoczki – każdy nowy punkt jest dodawany tylko wtedy, gdy nie powoduje prawego skrętu, co gwarantuje, że powstała figura jest wypukła i obejmuje wszystkie punkty wejściowe.

**Złożoność:**  
$O(N \log N)$

**Pseudokod:**
```py
```py
sort points lexicographically by (x, y)
lower = []
for p in points:
    while len(lower) >= 2 and cross(lower[-2], lower[-1], p) <= 0:
        lower.pop()
    lower.append(p)

upper = []
for p in reversed(points):
    while len(upper) >= 2 and cross(upper[-2], upper[-1], p) <= 0:
        upper.pop()
    upper.append(p)

# Concatenate lower and upper, removing the last point of each (it's duplicated)
convex_hull = lower[:-1] + upper[:-1]
```
```
