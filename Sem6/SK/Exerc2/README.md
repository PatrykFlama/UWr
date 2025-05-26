# Ćwiczenia 2
## Zadanie 1
$S = 10Mbit/s = 10^7b/s$  
$d = 2*2500m = 5000m$  
$t_{propagacji} = \frac{d}{v} = \frac{5000m}{10^8m/s} = 5*10^{-5}s$  
$d_{min} = t_{propagacji}*S = 5*10^{-5}s * 10^7b/s = 500b= 62.5B$  
rozmiar ramki musi być potęgą liczby 2, więc minimalny rozmiar ramki to 64B  

## Zadanie 2
$P(p, n) = n*p * (1-p)^{n-1}$  
wiemy że ta funkcja jest największa dla $p = \frac{1}{n}$, więc  
$P(\frac{1}{n}, n) = n * \frac{1}{n} * (1 - \frac{1}{n})^{n-1} = (1 - \frac{1}{n})^{n-1}$  
$\lim_{n \to \infty} (1 - \frac{1}{n})^{n-1} = e^{-1}$  
Więc oczekiwana liczba prób to $e$

## Zadanie 3
$1010_2$  

wielomian $x^2+x+1$  
dzielnik: $111$  
```
101000
111
______
010000
 111
______
001100
  111
______
  0010
```

wynik: 10

wielomian $x^7+1$  
dzielnik $10000001$  

```
10100000000
10000001
___________
00100001000
  100000010
___________
  000001010
```

wynik: 1010


## Zadanie 4
dostajemy pewien ciąg bitów

## Zadanie 5
$G(x) = x^n + \dots + 1$  
G(x) ma czynnik $x^0$ więc będie dzielił tylko te wielomiany, które mają czynnik $x^0$  
niech M(x) to pierwotna wiadomosć, a E(x) to wielomian błędu  
$T(x) = M(x) * x^n + R(x)$  
jeżeli G(x) dzieli E(x) to błąd nie zostanie wykryty  
$T(x) = T(x) + E(x)$  

## Zadanie 8
zmieniamy kod długości 4 na długości 7 (czyli dodajemy 3 bity)  
Jeśli mamy kodowanie gwarantujące, że odległość Hamminga między dowolną parą kodów to co najmniej k:
- potraﬁmy wykryć do k-1 błędów pojedynczych bitów,
- potraﬁmy skorygować do (k-1)/2 błędów pojedynczych bitów

W kodowaniu hamminga(7,4) trzymamy 3 dodatko bity parzystości dla 3 różnych trójek  
wystardczy więc pokazać że hamming(7,4) ma odległości co najmniej 3  
mamy $2^7=128$ słów 7-bitowych  
możemy zakodować $2^4=16$ słów 4-bitowych  
każde słowo 4-bitowe ma 7 sąsiadów 7-bitowych (błędów 1-bitowych)  
więc mamy $16*8=2^4*2^3=2^7$ słów, co pokrywa sałą przedstrzeń  
jeżeli przestrzeń jest dokładnie pokryta kulami o promieniu 1 to nie mogą się one nakładać, więc możymy skorygować 1 błąd  
dwa słowa nie mogą mieć odległości mniejszej niż 3, bo wtedy miałyby więcej niż 7 sąsiadów

## Zadanie 9
### a)
jeden bit ma prawdopodobieństwo $\frac{1}{100}$ błędu  
$P = (\frac{99}{100})^100 \approx 0.37$  

### b)
będziemy mieli $\frac{100}{4}*7=175$ bitów w 25 blokach  
ppb sukcesu bloku to $P_{0 błędów} + P_{1 błąd}$  
$P_{0 błędów} = \frac{99}{100}^7$  
$P_{1 błąd} = \binom{7}{1} * \frac{1}{100} * (\frac{99}{100})^6 = 7 * \frac{1}{100} \frac{99}{100}^6$  

$P_{sukcesu} \approx 0.998$  
$(0.998)^{25} \approx 0.95$  

## Zadanie 10

