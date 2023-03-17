# Statystyki pozycyjne
###### Problem code: SELECT \| Time: 3 s \| Memory: 16 MB \| Solved: yes \| print

Zrealizuj strukturę danych, w której będą możliwe następujące operacje:

* insert(x) - wstaw element x, o ile nie jest on jeszcze w strukturze,
* erase(x) - usuń element x
* select(k) - zwróć k-ty co do wielkości element

Za pomocą swojej struktury zasymuluj jej działanie dla przykładowych operacji podanych na standardowym wejściu. Instrukcje będą zapisane w osobnych wierszach i oznaczają odpowiednio:

* A <liczba> - insert(<liczba>)
* E <liczba> - erase(<liczba>)
* S <liczba> - select(<liczba>)

#### Wejście
W pierwszym wierszu dana jest liczba zestawów danych. Każdy zestaw to ciąg instrukcji zapisanych w osobnych wierszach, zakończony instrukcją K 0. Możesz założyć, żę każda operacja insert(x) będzie miała 0 ≤ x ≤ 1000000.   
Przykład:
```
2
A 10
S 4
A 101
A 1
A 13
A 14
A 154
A 2340
A 13240
E 222
K 0
A 10
A 101
A 1
A 13
A 14
A 154
A 2340
A 13240
E 222
S 1
S 2
S 3
S 4
S 5
K 0
```

#### Wyjście
Dla każdego zestawu danych wypisz odpowiedzi do instrukcji zgodnie z poniższymi zasadami:
* insert(x) - nic nie wypisuj
* erase(x) - jeśli elementu x nie było w strukturze, to należy wypisać słowo brak
* select(k) - wypisz k-ty element w strukturze; jeśli takiego nie ma, to należy wypisać słowo brak

Przykład:
```
brak
brak
brak
1
10
13
14
101
```