# Ćwiczenia 2
## Zadanie 1
$S = 10Mbit/s = 10^7b/s$  
$d = 2*2500m = 5000m$  
$t_{propagacji} = \frac{d}{v} = \frac{5000m}{10^8m/s} = 5*10^{-5}s$  
$d_{min} = t_{propagacji}*S = 5*10^{-5}s * 10^7b/s = 500b= 62.5B$  
rozmiar ramki musi być potęgą liczby 2, więc minimalny rozmiar ramki to 64B  

## Zadanie 2
$P(p, n) = [\text{liczba uczestików}] * [\text{ppb 1 osoba nadała}] * [\text{ppb n-1 osób nie nadało}]$  
$P(p, n) = n * p * (1-p)^{n-1}$  
szukamy maksimum  
$\frac{dP}{dp} = - n (1-p)^{n-2} (np - 1) = 0$  
$np(1-p)^{n-2} = (1-p)^{n-2}$  
$p = \frac{1}{n}$  
więc P osiąga maksimum dla $p = \frac{1}{n}$  
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
_______
010000
 111
_______
001100
  111
_______
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
CRC-1 z $G(x) = x+1$ działa identycznie jak bit parzystości  
weźmy jakiś wielomian P(x)  
$xP(x) = G(x)R(x) + b$ gdzie $b \in {0, 1}$  
pokażemy że $b=1$ wtw P ma nieparzyście wiele wsp różnych od 0  
indukcja po stopniu P(x)    
- baza:   
stopień 0, czyli P(x) = 0, wtedy $R(x)=0, b= 0$  
stopień 1, P(x) = 1, wtedy $R(x)=1, b= 1$  

- indukcja, zał że zachodzi dla deg $\leq n$  
wtedy gdy P(x) ma deg n+1 to $P(x) = x^{n+1} + Q(x)$  
$x P(x) = x^{n+2} + x Q(x) = x^{n+1}(x+1) + x(x^n + Q(x))$  
jeżeli Q ma niep wiele wsp to b = 0, wtedy P ma parz wiele wsp  
jeżeli Q ma parz wiele wsp to b = 1, wtedy P ma nieparz wiele wsp  

___
rozw V2:  

weźmy sobie $x^{j+k} + x^j = x^j (x+1) (x^{k-1} + \dots + x^0)$


## Zadanie 5
$G(x) = x^n + \dots + 1$  

błąd E(x) możemy zapisać jako $E(x)=x^j A(x)$ gdzie $A(x)$ jest wielomianem o stopniu $\leq n-1$, a j to pozycja ostatniego błędu  

d-d nie wprost:  
błąd nie został wykryty jeżeli G(x) dzieli E(x)  
G(x) dzieli E(x) jeżeli G(x) dzieli $x^j$ lub A(x)  
- G(x) nie dzieli $x^j$ bo występuje w nim $x^0$  
- G(x) nie dizeli A(x) bo A(x) ma stopień mniejszy niż G(x) (G ma stopień co najmniej n, a A(x) ma stopień mniejszy niż n)     

otrzymujemy sprzeczność, więc jeżeli G(x) dzieli E(x) to wykryliśmy błąd  

jeżeli G(x) nie zawiera $x^0$ to pierwszy podpunkt (G(x) dzieli x^j) może zajść i błąd nie zostanie wykryty  


## Zadanie 6
$G(x) = x^3 + x + 1$  
pokazać że wykryje wszystkie błędy oddalone o nie więcej niż 6 bitów  
$E(x) = (x^k + 1) x^j \space\space\space 1 \leq k \leq 6$
- G(x) nie dzieli $x^j$  ($x^j = G(x) Q(x)$)
- G(x) nie dzieli $x^k + 1$ (co można np sprawdzić na palcach)
  - $x+1$ mniejszy stopień
  - $x^2+1$ mniejszy stopień
  - $x^3+1$ reszta $x$
  - $x^4+1$ reszta $x^2+x+1$
  - $x^5+1$ reszta $x^2+x$
  - $x^6+1$ reszta $x^2$

- $x^j$ jest względnie pierwzsa z $x^k+1$

więc możemy wywnioskować że G(x) nie dzieli $E(x)$, więc wykryjemy błąd

## Zadanie 7
wiemy że jest max 1 błąd

$E(x) = x^j; 0 \leq j \leq 6 \lor E(x) = 0$

- G(x) dzieli B'(x) to ok
- G(x) nie dzieli B'(x) to:   

spróbujemy dodać do wiadomości $x^j$  
- jeżeli G(x) nie dzieli $B'(x)+x^j$   to dodał złe $x^j$ i nie otrzymaliśmy oryginalnej wiadomości (wiemy to z zad 6) (otrzymaliśmy $B(x) + x^i + x^j$)
- jeżeli G(x) dzieli $B'(x)+x^j$ to chcemy się upewnić że dodaliśmy poprawne $x^i$  
jeżeli $i\neq j$ to z zad 6 wiemy że G(x) nie dzieliłoby takiego wielomianu  
więc otrzymaliśmy $B'(x) + x^i = B(x)$  


## Zadanie 8
zmieniamy kod długości 4 na długości 7 (czyli dodajemy 3 bity)  
Jeśli mamy kodowanie gwarantujące, że odległość Hamminga między dowolną parą kodów to co najmniej k:
- potraﬁmy wykryć do k-1 błędów pojedynczych bitów,
- potraﬁmy skorygować do (k-1)/2 błędów pojedynczych bitów

W kodowaniu hamminga(7,4) trzymamy 3 dodatko bity parzystości dla 3 różnych trójek  
wystardczy więc pokazać że hamming(7,4) ma odległości co najmniej 3  
mamy $2^7=128$ słów 7-bitowych  
możemy zakodować $2^4=16$ słów 4-bitowych  
każde słowo 4-bitowe w przestrzeni 7-bitowej (po dodaniu bitów parzystości) ma 7 sąsiadów 7-bitowych (błędów 1-bitowych)  
więc mamy $[\text{ile mamy słów 4-bitowych}]*[\text{ile mamy punktów 'wokół' nich}] = 16*8=2^4*2^3=2^7$ słów, co pokrywa sałą przedstrzeń  
jeżeli przestrzeń jest dokładnie pokryta kulami o promieniu 1 to nie mogą się one nakładać, więc możymy skorygować 1 błąd  

## Zadanie 9
### a)
jeden bit ma prawdopodobieństwo $\frac{1}{100}$ błędu  
$P = (\frac{99}{100})^{100} \approx 0.37$  

### b)
będziemy mieli $\frac{100}{4}*7=175$ bitów w 25 blokach  
ppb sukcesu bloku to $P_{0 błędów} + P_{1 błąd}$  
$P_{0 błędów} = \frac{99}{100}^7$  
$P_{1 błąd} = \binom{7}{1} * \frac{1}{100} * (\frac{99}{100})^6 = 7 * \frac{1}{100} \frac{99}{100}^6$  

$P_{sukcesu} \approx 0.998$  
$(0.998)^{25} \approx 0.95$  

> zazwyczaj w warstwie fizycznej siedzą kody Richarda-Solomona

## Zadanie 10
mamy funkcję hashującą $h: U \to \{0,2^n-1\}$  
mamy $2^{m/2}$ różnych losowych tekstów i liczymy $h(x)$ dla każdego z nich  
chcemy pokazać że z ppb $\Omega(1)$ znajdziemy kolizję  
$\exists c > 0 \forall m p_m \geq C$

$p_m= \frac{2^m}{2^m} * \frac{2^m-1}{2^m} * \dots * \frac{2^m-2^{m/2}+1}{2^m} =$
$=  \frac{2^m!}{2^{m*2^{m/2}}(2^m-2^{m/2})!} = \frac{n!}{n^{\srqt{n}(n-\sqrt{n})!}}  
z wolframa wychodzi $\frac{1}{e}$  ($n = 2^m$)


$\lim_{m \to \infty} p_m = \frac{1}{\sqrt{e}} < 1$  
