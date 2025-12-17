# List 4.5

| 1 | 2 | 3 | 4 | 5 | 6 | 7* | 8 | 9 | 10 | 11* | 12* |
|---|---|---|---|---|---|----|---|---|----|-----|-----|
| x | x | x | x | x |   | x  |   |   | 2  |     |     |

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
zasada bayesa: 
$$P(C_k|X) = \frac{P(X|C_k)P(C_k)}{P(X)}$$  

co oznacza że  
$$P(X|C_k) = P(x_1 | x_2, ..., x_n, C_k) P(x_2 | x_3, ..., x_n, C_k) ... P(x_n | C_k)$$

naiwny klasyfikatorr bayesowski zakłada że cechy są niezależne warunkowo względem klasy:  

$$P(x_i | x_{i+1},...,x_n |  C_k) = P(x_i | C_k)$$

więc

$$P(X|C_k) = P(x_1 | C_k) P(x_2 | C_k) ... P(x_n | C_k)$$

oraz

$$P(C_k|X) = \frac{P(C_k) \prod_{i=1}^{n} P(x_i | C_k)}{P(X)} \propto P(C_k) \prod_{i=1}^{n} P(x_i | C_k)$$

dlatego BERT jest lepszy:
- jest on w stanie uchwycić zależności między cechami (słowami) w zdaniu
- uczy się reprezentacji słów w kontekście całego zdania, a nie tylko na podstawie ich częstotliwości występowania w klasach (np widzi podobieństwa między słowami)


## Zad 8

## Zad 9


## Zad 10
modelowanie języka python 

1. czy standardowa tokenizacja jest dobrym wyborem?
raczej nie - wolelibyśmy w tym wypadku tokenizację opartą na składni języka programowania, aby uchwycić struktury takie jak bloki kodu, funkcje, klasy itp.  
np chcielibyśmy mieć osobne specjalne tokeny dla słów kluczowych i operatorów

2. jak najlepiej obsłużyć wcięcia w kodzie?
można wprowadzić specjalne tokeny reprezentujące poziomy wcięć (otwarcie i zamknięcie wcięcia) lub użyć tokenów reprezentujących poziom wcięcia

3. czym jest pep-8 i czy można go wykorzystać?
PEP-8 to standard opisujący styl kodu w Pythonie  
możemy z niego skorzystać, aby uczyć model generowania kodu zgodnego z tym standardem - co poprawi uczenie (jeden styl, dodatkowo model nie musi uczyć się różnych wariantów formatowania kodu)

4. Jakie są argumenty za tym, że warto zmieniać nazwy zmiennych/funkcji/klas/... w kodzie (zachowując jego semantykę)?
- jest to forma augmentacji danych
- zmniejsza efekt przeuczenia, ucząc model aby nie skupiał się na konkretnych nazwach

5.  Jak metody NLP (word2vec?, transformery?, ...) mogą pomóc w zamianie nazw (podaj co najmniej dwa przykłady)
- można użyć osadzeń słów do znalezienia podobnych nazw
- można użyć modeli transformerowych do zrozumienia kontekstu, w którym nazwy są używane, co pozwala na generowanie bardziej odpowiednich zamienników

6. Jak statyczna analiza kodu może pomóc w tym zadaniu? (wystarczy jeden scenariusz)

statyczna analiza kodu pozwoli na bezpieczne modyfikowanie kodu, bez ryzyka wprowadzenia błędów, poprzez analizę zależności i użycia zmiennych/funkcji/klas w kodzie

więc np możemy bezpiecznie zmienić nazwę zmiennej




