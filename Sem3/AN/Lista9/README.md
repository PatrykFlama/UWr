[(wróć)](../)

# Lista 9
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
|---|---|---|---|---|---|---|---|---|
|   |   |   |   |   |   |   |   |   |


## Zadanie 2
Wielomian Bernsteina $B^n_k$ możemy zdefiniować kombinatorycznie:  
niech _k_ będzie liczbą pewnych konkretnych wygranych gier w _n_ grach, a _x_ prawdopodobieństwem wygrania gry.  
Wtedy możemy zdefiniować prawdopodobieństwo wygrania _k_ gier w _n_ rozróżnialnych grach jako  
$$B^n_k(x) = \binom{n}{k}x^k(1-x)^{n-k}$$  

#### (a) $B^n_i$ jest nieujemny w przedziale [0, 1] i osiąga w nim dokładnie jedno maksimum
jeżeli weźmiemy poprawne prawdopodobieństwo x (z przedziału [0, 1]) to nasza kombinatoryczna interpretacja jest poprawna, a prawdopodobieństwo jest nieujemne. Osiąga on dokłanie 1 maksimum wtedy, kiedy prawdopodobieństwo wygrania gry _x_ = $\frac{k}{n}$, bo wtedy mamy największą szansę na wygranie _k_ gier

#### (b) $\sum_{i=0}^n B^n_i(u) = 1$
jeżeli weźmiemy prawdopodobieństwo x z przedziału [0, 1] to prawdopodobieństwo wygrania dowolnej liczby gier z _n_ gier wynosi 1, a te prawdopodobieństwo to suma prawdopodobieństw po wszystkich k - czyli po wielomianach Bernsteina. 

#### (c) $\sum_{i=0}^n \frac{i}{n}B^n_i(t) = t$
$$\frac{k}{n} B^n_k(x) = \frac{k}{n} \binom{n}{k}x^k(1-x)^{n-k} = \frac{n!}{k!(k-1)!} \frac{k}{n} ... = \frac{(n-1)!}{(k-1)!(n-k)!} x^{k-1}x(1-x)^{n-k} = x B^n_k$$
$$\sum_{i=0}^n \frac{i}{n}B^n_i(t) = x \sum_{i=0}^n B^n_k = x$$

#### (d) $B_i^n(u) = \frac{n+1-i}{n+1} B_i^{n+1}(u) + \frac{i+1}{n+1} B_{i+1}^{n+1}(u)$ dla $0 \leq i \leq n$
