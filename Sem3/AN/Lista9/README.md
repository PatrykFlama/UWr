[(wróć)](../)

# Lista 9
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
|---|---|---|---|---|---|---|---|---|
| X | X | X | X | X | X | X | X | X |

## Zadanie 1
![image](./zad1.png)

## Zadanie 2
#### (a) $B^n_i$ jest nieujemny w przedziale [0, 1] i osiąga w nim dokładnie jedno maksimum
Wielomian Bernsteina $B^n_k$ możemy zdefiniować kombinatorycznie:  
niech _k_ będzie liczbą pewnych konkretnych wygranych gier w _n_ grach, a _x_ prawdopodobieństwem wygrania gry.  
Wtedy możemy zdefiniować prawdopodobieństwo wygrania _k_ gier w _n_ rozróżnialnych grach jako  
$$B^n_k(x) = \binom{n}{k}x^k(1-x)^{n-k}$$  

jeżeli weźmiemy poprawne prawdopodobieństwo x (z przedziału [0, 1]) to nasza kombinatoryczna interpretacja jest poprawna, a prawdopodobieństwo jest nieujemne. Osiąga on dokłanie 1 maksimum wtedy, kiedy prawdopodobieństwo wygrania gry _x_ = $\frac{k}{n}$, bo wtedy mamy największą szansę na wygranie _k_ gier

#### (b) $\sum_{i=0}^n B^n_i(u) \equiv 1$
Dla dowolnego _u_:  
$$\sum_{i=0}^n B^n_i(u) = \sum_{i=0}^n \binom{n}{i}u^i(1-u)^{n-i} = (u + 1 - u)^n = 1$$

#### (c) $\sum_{i=0}^n \frac{i}{n}B^n_i(t) = t$
$$\frac{k}{n} B^n_k(x) = \frac{k}{n} \binom{n}{k}x^k(1-x)^{n-k} = \frac{n!}{k!(k-1)!} \frac{k}{n} ... = \frac{(n-1)!}{(k-1)!(n-k)!} x^{k-1}x(1-x)^{n-k} = x B^{n-1}_{k-1}(x)$$
$$\sum_{k=0}^n \frac{k}{n}B^n_k(t) = x \sum_{k=0}^n B^{n-1}_{k-1}(t) = x[B^{-1}_{-1}(t) + \sum_{k=0}^{n-1} B^{n-1}_{k}(t)] = x$$

#### (d) $B_i^n(u) = \frac{n+1-i}{n+1} B_i^{n+1}(u) + \frac{i+1}{n+1} B_{i+1}^{n+1}(u)$ dla $0 \leq i \leq n$
$$B^n_i(u)=\binom{n}{i}u^i(1-u)^{n-i}(1-u+u)=\binom{n}{i}u^i(1-u)^{n+1-u} + \binom{n}{i}u^{i+1}(1-u)^{n-i}=$$
$$=\frac{n+1-1}{n+1}\binom{n+1}{i}u^i(1-u)^{n+1-i}+\frac{i+1}{n+1}\binom{n+1}{i+1}u^{i+1}(1-u)^{n-i}=\frac{n+1-i}{n+1}B^{n+1}_i(u)+\frac{i+1}{n+1}B^{n+1}_{i+1}$$

## Zadanie 3
![image](./zad3.png)

## Zadanie 4
![image](./zad4.png)

## Zadanie 5
![image](./zad5.png)

## Zadanie 6
![image](./zad6.png)

## Zadanie 7
![image](./zad7.png)

## Zadanie 8
![image](./zad8.png)
