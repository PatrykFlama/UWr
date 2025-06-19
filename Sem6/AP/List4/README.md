[(back)](../)

# Lista 4 - Techniki pierwiastkowe

## Zad 1 - Dynamiczne sumy na przedziałach
### Opis:
Mamy typy zapytań:
- `1 k u`: aktualizacja wartości
- `2 a b`: suma wartości na `[a, b]`

Zastosowana struktura: drzewo pierwiastkowe (Sqrt Decomposition)

### Złożoności:
- Budowanie: $O(n)$
- Aktualizacja: $O(1)$
- Zapytanie: $O(\sqrt n)$

### Koncept działania:
- Dzielimy tablicę na bloki o rozmiarze $\sqrt n$
- Każdy blok przechowuje sumę swoich elementów
- Aktualizacja elementu w bloku aktualizuje sumę tego bloku
- Zapytanie sumy na przedziale łączy sumy odpowiednich bloków i reszty



## Zad 2 - Skoki
### Opis: 
Oblicz sumy typu: `x[a] + x[a+b] + x[a+2b] + ...`

### Technika:
- Dla małych b ($b \leq sqrt(n)$) - preprocesowanie sum w $O(n*\sqrt n)$  
- Dla dużych b - iteracyjne liczenie na żywo $O(n/b)$ (co w sumie nie przekroczy $O(n*\sqrt n)$ w całości)  


## Zad 3 - Punkty na płaszczyźnie
### Opis: 
Uporządkuj punkty $(x, y)$ w takiej kolejności, by suma dystansów Manhattan'skich nie przekroczyła _2.1e9_

### Metoda:
- dzielimy płaszczyznę na pasy o pierwiastkowej szerokości (np pasy horyzontalnie)
- sortujemy punkty w każdym pasie (względem długości pasa, np po x)
- punkty w pasach łączymy wg posortowanej kolejności, a wychodząc z pasa przechodzimy do kolejnego (do bliższego końca)

złożoność (w przykładowym podziale na pasy):  
```
niech L - liczba pasów

<dystans pokonany w oy> + <dystans pokonany w ox> = 
<dysans oy w pasie> + <dystans oy łączący pasy> + <dysans ox w pasie> + <dystans ox łączący pasy> =
(L * L/sqrt(L)) + (2 * L/sqrt(L) * sqrt(L)) + (L * sqrt(L)) + 0 = 
L * sqrt(L) + 2 * L = O(L * sqrt(L))
```

## Zad 4 - Punkty stałe
### Opis:
Ile liczb x występuje dokładnie x razy w przedziale [p, k]

### Technika:
Sztuczka Mo ($O((n + q) * \sqrt n)$); polega ona na:
sortowaniu zapytań w taki sposób, by minimalizować liczbę zmian w przedziale podczas odpowiadania na nie - 
będziemy  używać dwóch wskaźników do przedziału, które przesuwają się w lewo i prawo, a także utrzymują aktualny stan odpowiedzi  
  
- będziemy sortoważ zapytania $O(q \log q)$
- dla każdego zapytania, umieszczamy lewy kraniec w którymś z $\sqrt n$ bloków
- prawy kraniec jest posortowany
- dzięki temu prawy kraniec przesuniemy sumarycznie $O(n)$ razy, a lewy kraniec dla każdego zapytania przejdzie maksymalnie $O(\sqrt n)$ razy, co daje łącznie $O(n * \sqrt n)$ operacji dla wszystkich zapytań w bloku
- po wykonaniu operacji na wszsystkich blokach, wykonaliśmy
  - $O(\sqrt n)$ przesunieć lewego krańca per zapytanie 
  - $O(\sqrt n * n)$ przesunięć prawego krańca 
- zatem złożoność to $O((n + q) * \sqrt n)$



### Pseudokod:
```py
def mo(queries):
    queries.sort(key=lambda q: (q.l // sqrt_n, q.r))
    
    for query in queries:
        while l < query.l:
            remove(arr[l])
            l += 1
        while l > query.l:
            l -= 1
            add(arr[l])
        while r < query.r:
            add(arr[r])
            r += 1
        while r > query.r:
            r -= 1
            remove(arr[r])

        results[query.id] = result
```


