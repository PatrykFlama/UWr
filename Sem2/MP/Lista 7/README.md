[(wróć)](../)

# Lista 7
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|---|---|---|---|---|---|---|---|
|   |   |   |   |   |   |   |   |

<details>
<summary> Info o pozytywnych i negatywnych wystąpieniach w kontraktach </summary>

Intuicja jest taka, że wystąpienia **pozytywne** opisują wartości **dostarczane przez funkcję** której dotyczy kontrakt, a **negatywne** - wartości **dostarczane przez użytkownika**. Formalnie _"wystąpienie jest pozytywne"_ to takie które jest na pozycji argumentu parzyście wielu strzałek (zero też jest liczbą parzystą), np.\
W kontrakcie (-> (-> A B) C D) wystąpienia **pozytywne** to **D** (nie jest argumentem żadnej strzałki, tylko na pozycji odpowiadającej zwracanej wartości) oraz A (jest argumentem dla obu strzałek, a dwa jest liczbą parzystą).\
Wystąpienia **negatywne** to **B** (jest "zwracaną wartością" strzałki będącej na pozycji argumentu) i **C** (jest na pozycji argumentu).\
W powyższym przykładzie ciekawe jest to, że **A** jest na pozycji **pozytywnej**, choć nie opisuje zwracanej wartości. Żeby jakaś wartość została sprawdzona ze względu na kontrakt A, musi być przekazana do funkcji (tej opisanej kontraktem (-> A B)) przez funkcję, której dotyczy cały kontrakt. Dlatego choć nie jest to zwracana wartość, to jest dostarczana przez funkcję opisaną kontraktem (-> (-> A B) C D).\
Z punktu widzenia kontaktów, polarność wystąpienia (czyli to, czy jest ono pozytywne, czy negatywne) ma istotne znaczenie dla "blamingu", czyli wskazania strony, która złamała kontrakt. Jest też potrzebne przy implementacji kontraktów parametrycznych. Jest to jednak pojęcie, które z punktu widzenia kontaktów, polarność wystąpienia (czyli to, czy jest ono pozytywne, czy negatywne) ma istotne znaczenie dla "blamingu", czyli wskazania strony, która złamała kontrakt. Jest też potrzebne przy implementacji kontraktów parametrycznych.\
Jest to jednak pojęcie, które przewija się przez wiele innych zagadnień z pogranicza logiki i języków programowania, dlatego poświęcamu mu trochę uwagi na tym przedmiocie.

</details>

## Zadanie 1


## Zadanie 2


## Zadanie 3


## Zadanie 4


## Zadanie 5


## Zadanie 6


## Zadanie 7


## Zadanie 8



