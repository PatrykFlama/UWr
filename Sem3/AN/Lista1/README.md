[(wróć)](../)

# Lista 1
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|---|---|---|---|---|---|---|---|
| X | X | X | X |   | X | X | X |

## Zadanie 1
zfałszowane działanie



## Zadanie 6
$\pi = 4 \sum_{k=0}^{\infty} \frac{(-1)^k}{2k+1}$  
monotoniczne, szereg naprzemienny, więc szukamy takiego k, że $\frac{4}{2k+1} < 10^{-6}$  
$\frac{4}{2k+1} < 10^{-6}$  
$2k+1 > 4 * 10^6$  
$k > 2 * 10^6 - 1$  
więc najmniejsze $k = 2 * 10^6$

## Zadanie 7
$\ln(x) = \sum_{k=1}^{\infty} (-1)^{k-1} \frac{(x-1)^k}{k}$  
$\ln(2) = \sum_{k=1}^{\infty} (-1)^{k-1} \frac{1^k}{k} = \sum_{k=1}^{\infty} (-1)^{k-1} \frac{1}{k}$  
$\frac{1}{k}$ monotoniczne, szereg naprzemienny, więc szukamy takiego k, że $\frac{1}{k} < [błąd]$  
$\frac{1}{k} < \frac{1}{2} * 10^{-6}$  
$k < 2 * 10^6$  

natomiast korzystając z $\ln 2 = \ln(e(\frac{2}{e}))$  
$\ln 2 = \ln(e(\frac{2}{e})) = \ln(e) + \ln(\frac{2}{e}) = 1 - \sum_{k=1}^{\infty} (-1)^{k} \frac{(\frac{2}{e}-1)^k}{k}$  
$\sum_{k=1}^{\infty} (-1)^{k} \frac{(\frac{2}{e}-1)^k}{k} = \sum_{k=1}^{\infty} \frac{(1-\frac{2}{e})^k}{k}$  
$\frac{(1-\frac{2}{e})^k}{k} < \frac{1}{k}$  
czyli dla danej iteracji _k_ drugi szereg będzie mniejszy od pierwszego, 
zarazem maleje on szybciej oraz jest monotoniczny, dodatni i ograniczony, 
więc szybciej osiągnie mały błąd  

## Zadanie 8
Mamy funkcję $\arctan$, która zwraca dokładny wynik tylko dla $x \in [-1, 1]$, 
skorzystamy z własności   
$\arctan(x) + \arctan(\frac{1}{x}) = \frac{\pi}{2}$; $x\in\mathbb{R_+}$  
$\arctan(x) + \arctan(\frac{1}{x}) = -\frac{\pi}{2}$; $x\in\mathbb{R_-}$  
czyli dla $x \in \mathbb{R}$  
```py
def arctan(x):
    if x > 1:
        return pi/2 - PWOpp_arctan(1/x)
    elif x < -1:
        return -pi/2 - PWOpp_arctan(1/x)
    else:
        return PWOpp_arctan(x)
```
