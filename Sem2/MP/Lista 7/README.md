[(wróć)](../)

# Lista 7
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|---|---|---|---|---|---|---|---|
| X |   | X | X | X | X |   |   |

<details>
<summary> Info o pozytywnych i negatywnych wystąpieniach w kontraktach </summary>

Intuicja jest taka, że wystąpienia **pozytywne** opisują wartości **dostarczane przez funkcję** której dotyczy kontrakt, a **negatywne** - wartości **dostarczane przez użytkownika**. Formalnie _"wystąpienie jest pozytywne"_ to takie które jest na pozycji argumentu parzyście wielu strzałek (zero też jest liczbą parzystą), np.\
W kontrakcie (-> (-> A B) C D) wystąpienia **pozytywne** to **D** (nie jest argumentem żadnej strzałki, tylko na pozycji odpowiadającej zwracanej wartości) oraz A (jest argumentem dla obu strzałek, a dwa jest liczbą parzystą).\
Wystąpienia **negatywne** to **B** (jest "zwracaną wartością" strzałki będącej na pozycji argumentu) i **C** (jest na pozycji argumentu).\
W powyższym przykładzie ciekawe jest to, że **A** jest na pozycji **pozytywnej**, choć nie opisuje zwracanej wartości. Żeby jakaś wartość została sprawdzona ze względu na kontrakt A, musi być przekazana do funkcji (tej opisanej kontraktem (-> A B)) przez funkcję, której dotyczy cały kontrakt. Dlatego choć nie jest to zwracana wartość, to jest dostarczana przez funkcję opisaną kontraktem (-> (-> A B) C D).\
Z punktu widzenia kontaktów, polarność wystąpienia (czyli to, czy jest ono pozytywne, czy negatywne) ma istotne znaczenie dla "blamingu", czyli wskazania strony, która złamała kontrakt. Jest też potrzebne przy implementacji kontraktów parametrycznych. Jest to jednak pojęcie, które z punktu widzenia kontaktów, polarność wystąpienia (czyli to, czy jest ono pozytywne, czy negatywne) ma istotne znaczenie dla "blamingu", czyli wskazania strony, która złamała kontrakt. Jest też potrzebne przy implementacji kontraktów parametrycznych.\
Jest to jednak pojęcie, które przewija się przez wiele innych zagadnień z pogranicza logiki i języków programowania, dlatego poświęcamu mu trochę uwagi na tym przedmiocie.

</details>

## Zadanie 4
**Pozytywne** (_parzyste_) dostarczone przez **funkcję**\
**Negatywne** (_nieparzyste_) dostarczone przez **użytkownika**
```
(parametric->/c [a b] (-> a b a))
```
pozytywne a2\
negatywne a1 b1
```
(parametric->/c [a b c] (-> (-> a b c) (-> a b) a c))
```
pozytywne a1 b1 a2 c2\
negatywne c1 b2 a3
```
(parametric->/c [a b c] (-> (-> b c) (-> a b) (-> a c)))
```
pozytywne b1 a1 a2\
negatywne c1 b2 c2
```
(parametric->/c [a] (-> (-> (-> a a) a) a))
```
pozytywne a2 a4\
negatywne a1 a3

## Zadanie 6
```
(parametric->/ c [a b] (-> (-> a b b) b (listof a) b))
((' a 'b -> 'b) 'b (Listof 'a) -> 'b)

(parametric->/ c [a a] (-> (-> a a a) a (listof a) a))
((' a 'a -> 'a) 'a (Listof 'a) -> 'a)
```
Nie, w najlepszym wypadku daje mu większe możliwości na wyjściu; posiadanie typów a' i 'b jest bardziej ograniczjające ponieważ na wyjściu musimy zwrócić typ b - akumulator, i nie możemy zwrócić elementu listy. Innymi słowami - nie rozbijając naszych typów wejściowych na 2 różne pudełka, możemy w wyniku wziąć dowolny typ z wejściowych, co daje nam większe możliwości niż gdybyśmy mogli wziąć dowolny typ z ograniczonego zbioru z wejściowych.\
Zarazem w tym konkretnym przypadku nie ma to znaczenia, z racji iż nasza wartość wynikowa jest wartością wynikową podanej do folda funkcji, czyli ma to ten sam efekt.\
Natomiast gdybyśmy mieli zmienioną wersję typu to mniej ograniczeni jesteśmy dając sobie więcej różnych typów, czyli używając zarówno a jak i b. Dzięki temu nie musimy robić wszystkich typów takich samych (jak by było dla samych a), tylko możemy dać różne typy dla a i b.




