# List 4.5

| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 |
|---|---|---|---|---|---|---|---|---|----|----|----|
| x | x |   |   |   |   |   |   |   |    |    |    |

## Zad 1
tak   

- zdania z podobnymi znaczeniami będą blisko siebie - przy wielu takich zestawach podobne słowa też często będą występować blikso siebie



## Zad 2
zakładając że mamy jakiś korpus - lematyzujemy go i losowo tłumaczymy słowa na drugi język (utrzymując ich zlematyzowaną formę)  
w efekcie model będzie rozumieć zdania składające się z mieszaniny polskich i angielskich słów  

możemy też po prostu trenować model na korpusach - osobno oryginalnym polskim i angielskim (utworzonym poprzez lematyzację i tłumaczenie)  

co robić z wieloznacznością tłumaczeń? z racji iż nie mamy lepszego sposobu na tłumaczenie, możemy po prostu losowo wybierać jedno z możliwych tłumaczeń



## Zad 5
bez osadzeń pozycyjnych model nie zna kolejności poprzedzających słów w zdaniu, więc może się mylić w przypadkach gdzie to ma znaczenie, ale taki model nadal może być użyteczny do
- zadań kalsyfikacyjnych (np analiza sentymentu, gdzie samo wystąpienie negatywnych wyrazów jest zazwyczaj dostatecznym dowodem na wynik)
- 

duża liczba tokenów - dłuższy czas trenowania oraz większe wymagania pamięciowe/obliczeniowe, ale możliwość uchwycenia większej liczby nowych słów



