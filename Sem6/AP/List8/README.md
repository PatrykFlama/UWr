[(back)](../)

# Lista 8 - drzewa przedziałowe
## Zad 1 - Przydział do hoteli
Dla każdej grupy turystów należy znaleźć pierwszy hotel z odpowiednią liczbą pokoi i zmniejszyć jego dostępność

### Koncept:
Segment tree trzyma max liczby wolnych pokoi w przedziale. Aby znaleźć pierwszy hotel, który ma co najmniej `x` pokoi, wykonujemy zmodyfikowane wyszukiwanie (binary search w drzewie). Po przydzieleniu aktualizujemy drzewo.

### Złożoność czasowa:
$O(\log N)$ na zapytanie i aktualizację - łączna złożoność $O(Q \log N)$

### Pseudokod:
```py
build(a): // konstrukcja drzewa
query(x): // znajdź pierwszy indeks z rooms >= x
update(i, new_val): // zmniejsz rooms w hotelu i

for każda grupa z wymaganiem x:
    idx = query(x)
    if idx == -1:
        print 0
    else:
        update(idx, a[idx] - x)
        print idx+1
```



## Zad 2 - Usuwanie z listy
Usuwamy pozycje z dynamicznie zmieniającej się listy. Dla każdej pozycji `p`, wypisujemy wartość znajdującą się aktualnie na tej pozycji

### Koncept:
Drzewo segmentowe przechowuje liczbę aktywnych elementów w każdym przedziale `cnt`. Dla każdej pozycji `p` wykonujemy binary search w drzewie, by znaleźć faktyczny indeks, a następnie usuwamy go `cnt = 0`.

### Złożoność czasowa:
$O(\log N)$ na każde zapytanie - łączna złożoność $O(N \log N)$

### Pseudokod:
```py
build(a): Node(val=a[i], cnt=1)

find_kth(p): znajdź indeks i-ty aktywny element
    przechodź przez drzewo sprawdzając cnt lewej strony

update(i): ustaw cnt[i] = 0

for p in pozycje:
    idx = find_kth(p)
    print a[idx]
    update(idx)
```



## Zad 3 - Koleje
Rezerwacje są przyjmowane, jeśli na całym odcinku trasy od $P_i$ do $K_i$ jest wystarczająco dużo wolnych miejsc. Jeśli tak, liczba wolnych miejsc na tym odcinku zostaje zmniejszona.

### Koncept:
Drzewo segmentowe z leniwą propagacją, które przechowuje liczbę zarezerwowanych miejsc na każdym odcinku. Akceptacja odbywa się wtedy, gdy $\text{max_zajęte} + \text{nowe} \leq M$

### Złożoność czasowa:
$O(log N)$ na zapytanie i aktualizację - $O(Q log N)$

### Pseudokod:
```py
build(n): drzewa przechowujące max liczby miejsc zajętych

query(p, k): max zajętych miejsc w [p, k-1]

if query(p, k) + l <= M:
    update(p, k-1, +l) // zwiększ liczbę zajętych
    print 'T'
else:
    print 'N'
```


## Zad 4 - Aktualizacje i sumy na przedziałach
Mamy trzy typy operacji:
- dodaj `v` do każdego elementu w `[x, y]`
- ustaw `v` na każdym elemencie w `[x, y]`
- oblicz sumę w `[x, y]`

### Koncept:
Używamy drzewa przedziałowego z lazy propagation obsługującego zarówno operacje dodawania, jak i przypisania (ustawiania). Musimy rozróżniać operacje dodawania i ustawiania w propagacji.

### Złożoność czasowa:
$O(log N)$ na zapytanie i aktualizację - łączna $O(Q log N)$

### Pseudokod:
```py
Node: {sum, set_val (optional), add_val}

update_add(x, y, v):
    dodaj v do każdego elementu w [x, y]

update_set(x, y, v):
    ustaw wszystkie elementy na v

query(x, y):
    suma z [x, y]
```

