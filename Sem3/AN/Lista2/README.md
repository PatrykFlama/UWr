[(wróć)](../)

# Lista 1
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
|---|---|---|---|---|---|---|---|---|
|   |   |   |   | x |   | x |   |   |


## Zadanie 5
Standard IEEE 754 dzieli liczbę na 3 części: znak, wykładnik i mantysę   
mantysa jest zawsze zapisana w postaci 1,xxxxx... gdzie pierwsza 1 nie jest zapisywana    
Długość wykładnika i mantysy zależy od liczby bitów przeznaczonej na liczbę:  
![image](ieee754.png)

## Zadanie 7
Oblicazanie _f(0.001)_ daje niewiarygodny wynik, ponieważ $x^{14}$ dla małych _x_ wymaga dużej precyzji, a po dodaniu do tak małej liczby _1_ wynik jest zaokrąglany do _1_  
Aby policzyć wynik precyzyjnie wystarczy przekształcić wzór:  
$f(x) = 4046 \frac{\sqrt{x^{14}+1}-1}{x^{14}} = 4046 \frac{x^{14}}{x^{14}(\sqrt{x^{14}+1}+1)} = \frac{4046}{\sqrt{x^{14}+1}+1}$  
wtedy osiągamy lepszą precyzję dla małych x, ponieważ wtedy zaokrąglenie pierwiastka nie doprowadza do powstania 0 w liczniku, dodatkowo wykonujemy w nim mniej operacji zmniejszając końcowy błąd.  