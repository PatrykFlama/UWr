# List 4.5

| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 |
|---|---|---|---|---|---|---|---|---|----|----|----|
| x | x | x | x | x |   | x |   |   |    |    |    |

## Zad 1
tak   

- zdania z podobnymi znaczeniami będą blisko siebie - przy wielu takich zestawach podobne słowa też często będą występować blikso siebie



## Zad 2
trenujemy podobnie jak oryginalnego word2vec, ale dodatkowo chcemy aby lematy polskich wyrazów były blisko swoich angielskich odpowiedników

_____
zakładając że mamy jakiś korpus - lematyzujemy go i losowo tłumaczymy słowa na drugi język (utrzymując ich zlematyzowaną formę)  
w efekcie model będzie rozumieć zdania składające się z mieszaniny polskich i angielskich słów  

możemy też tworzyc korpus gdzie dla każdego polskiego słowa dopisujemy tłumaczenie jego lematu

co robić z wieloznacznością tłumaczeń? z racji iż nie mamy lepszego sposobu na tłumaczenie, możemy po prostu losowo wybierać jedno z możliwych tłumaczeń


## Zad 3
- (w stylu GPT) operujemy na jakimś prefiksie zdania, zamaskowane jest ostatnie słowo (lub kilka ostatnich), model je przewiduje i od nowa traktujemy je jako cały prefiks
- zaczynamy od losowego zdania, w każdej iteracji losujemy zamaskowane pozycje i model je przewiduje, powtarzamy to wiele razy aż zdanie się ustabilizuje



## Zad 4
warunki poezji:
- rymy - końcowe sylaby wybranych wersów muszą być takie same
- liczba sylab - każdy wers musi mieć określoną liczbę sylab


generujemy więc 'szablon' poezji, gdzie ustalamy końcową sylabę oraz liczbę sylab w każdym wersie  
następnie iteracyjnie losujemy pozycje i je przewidujemy, zachowując końcowe sylaby


## Zad 5
bez osadzeń pozycyjnych model nie zna kolejności poprzedzających słów w zdaniu, więc może się mylić w przypadkach gdzie to ma znaczenie, ale taki model nadal może być użyteczny do
- zadań kalsyfikacyjnych
  - analiza sentymentu, gdzie samo wystąpienie negatywnych wyrazów jest zazwyczaj dostatecznym dowodem na wynik
  - tematyka tekstu
  - autor tekstu


duża liczba tokenów 
- dłuższy czas trenowania oraz większe wymagania pamięciowe/obliczeniowe
- większa entropia - mamy więcej tokenów do wyboru, więc trudniej jest przewidzieć poprawny token
- możliwość uchwycenia większej informacji o słowie (przedrostki, końcówki, etc)


## Zad 6


## Zad 7
zasada bayesa: $P(A|B) = \frac{P(B|A)P(A)}{P(B)}$  
naiwny klasyfikatorr bayesowski zakłada że cechy są niezależne warunkowo względem klasy:  
$$P(C|F_1, F_2, ..., F_n) = \frac{P(C) \prod_{i=1}^{n} P(F_i|C)}{P(F_1, F_2, ..., F_n)}$$

dlatego BERT jest lepszy:
- jest on w stanie uchwycić zależności między cechami (słowami) w zdaniu
- uczy się reprezentacji słów w kontekście całego zdania, a nie tylko na podstawie ich częstotliwości występowania w klasach (np widzi podobieństwa między słowami)


## Zad 8

