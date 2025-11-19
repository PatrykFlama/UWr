# List 2


## Task  1
Wnioski do modelu _papugaGPT_ (wybranie mocniejszego modelu, takiego jak polka, **powinno** dać lepsze wyniki):  
Dla równań prostych - składających sie tylko z 2 wartości oraz prostych operatorów (dodawanie i odejmownie) i małych liczb - model jest w stanie kilkukrotniej dokładniej przybliżyć wynik niż losowe zgadywanie, a czasem nawet trafić w poprawną odpowiedź. Jednak po dołożeniu mnożenia lub odejmowania, papuga bardzo szybko (wraz ze wzrostem wartości liczb) traci jakąkolwiek trafność, a nawet daje gorsze przybliżenia niż losowe zgadywanie.   

Najwydajniejsze prompty dla tego modelu to równania w postaci *"Wartość wyrażenia matematycznego XXXX, to YYYY"*


`+, -`, range(1, 500):
```
Prompt 0: 0.00%, Avg answer distance: 228.6000  (expected random ~750)
        244+139 = "163.0" (383) -> dist: 220.0000
        196-137 = "-137.0" (59) -> dist: 196.0000
        293-318 = "-4.0" (-25) -> dist: 21.0000
        42+104 = "104.0" (146) -> dist: 42.0000
        341+354 = "354.0" (695) -> dist: 341.0000
        215+29 = "323.0" (244) -> dist: 79.0000
        312-360 = "-610.0" (-48) -> dist: 562.0000
        184+200 = "200.0" (384) -> dist: 184.0000
        143-235 = "-235.0" (-92) -> dist: 143.0000
        498+208 = "208.0" (706) -> dist: 498.0000
```

`+, -`, range(1, 1000):
```
Prompt 0: 0.00%, Avg answer distance: 495.2000 (expected random ~1500)
        730+190 = "1048.0" (920) -> dist: 128.0000
        817-846 = "-846.0" (-29) -> dist: 817.0000
        678+971 = "971.0" (1649) -> dist: 678.0000
        8-349 = "-9.0" (-341) -> dist: 332.0000
        694-847 = "8.0" (-153) -> dist: 161.0000
        873-761 = "-761.0" (112) -> dist: 873.0000
        946+127 = "127.0" (1073) -> dist: 946.0000
        599-736 = "-736.0" (-137) -> dist: 599.0000
        815-579 = "-10.0" (236) -> dist: 246.0000
        172+485 = "485.0" (657) -> dist: 172.0000
```

`*`, range(1, 10):
```
Prompt accuracies:                                                      
Prompt 0: 0.00%, Avg answer distance: 45.2000 (expected random ~50)
        3*9 = "9.0" (27) -> dist: 18.0000
        7*7 = "1.0" (49) -> dist: 48.0000
        10*9 = "1.0" (90) -> dist: 89.0000
        3*6 = "6.0" (18) -> dist: 12.0000
        8*10 = "10.0" (80) -> dist: 70.0000
        9*6 = "6.0" (54) -> dist: 48.0000
        8*3 = "3.0" (24) -> dist: 21.0000
        7*10 = "13.0" (70) -> dist: 57.0000
        7*7 = "7.0" (49) -> dist: 42.0000
        7*7 = "2.0" (49) -> dist: 47.0000
```


## Task 2
```
flax-community/papuGaPT2: 6/8 = 0.750
Final accuracy:  0.75
```


## Task  3

```
wprost uwielbiała słuchać o wartościach własnych macierzy
-54.63517761230469: wprost uwielbiał słuchać o wartościach własnych macierzy
-58.463905334472656: wprost uwielbiał słuchać o wartościach własnych mocarzy
-59.47153854370117: wprost wielbił słuchać o wartościach własnych macierzy
-59.47153854370117: wprost wielbił słuchać o wartościach własnych macierzy
-59.47153854370117: wprost wielbił słuchać o wartościach własnych macierzy
```

### concept 1:
greedy construct sentence and test all possible words for one place at a time - all other words should be either the chosen ones, or random  
additionally apply beam search to test _k_ best sentences

### concept 2:
genetic alg - choose at random _k_ samples, with each iteration generate _c*k_ new samples by random modifications: choose random word to modify, modify it at random. keep _k_ best samples

### concept 3:
keep _k_ samples  
calculate the words probability and choose the least probable one for modification (may be modified at random or using the most probable word)


## Task  4
> lets take the given corpus of prefixes ans calculate some _n-grams_ - then we can use them to generate fitting words, with guarantee tht we will only generate words starting on the same letter

load prefixes, choose one at random - then we could just take the tokens probability, but choose for random generation only those which start on same letter (kinda like task 2)



