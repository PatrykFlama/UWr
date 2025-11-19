# List 2


## Task  1
Wnioski do modelu _papugaGPT_ (wybranie mocniejszego modelu, takiego jak polka, **powinno** dać lepsze wyniki):  
Dla równań prostych - składających sie tylko z 2 wartości oraz prostych operatorów (dodawanie i odejmownie) i małych liczb - model jest w stanie kilkukrotniej dokładniej przybliżyć wynik niż losowe zgadywanie, a czasem nawet trafić w poprawną odpowiedź. Jednak po dołożeniu mnożenia lub odejmowania, papuga bardzo szybko (wraz ze wzrostem wartości liczb) traci jakąkolwiek trafność, a nawet daje gorsze przybliżenia niż losowe zgadywanie.   

Najwydajniejsze prompty dla tego modelu to równania w postaci *"Wartość wyrażenia matematycznego XXXX, to YYYY"*


`+`, range(1, 10):
```
Prompt accuracies:                                                                                                                                                       
Prompt 0: 20.00%, Avg answer distance: 9.8000
        1+10 = "10.0" (11) -> dist: 1.0000
        9+5 = "9.0" (14) -> dist: 5.0000
        2+7 = "1.0" (9) -> dist: 8.0000
        4+6 = "36.0" (10) -> dist: 26.0000
        1+8 = "9.0" (9) -> dist: 0.0000
        7+1 = "2.0" (8) -> dist: 6.0000
        7+5 = "12.0" (12) -> dist: 0.0000
        3+8 = "33.0" (11) -> dist: 22.0000
        1+10 = "1.0" (11) -> dist: 10.0000
        7+1 = "28.0" (8) -> dist: 20.0000

Prompt accuracies:                                                                                                                                                       
Prompt 0: 10.00%, Avg answer distance: 6.5000
        7 + 10 = "10.0" (17) -> dist: 7.0000
        6 + 4 = "39.0" (10) -> dist: 29.0000
        9 + 2 = "11.0" (11) -> dist: 0.0000
        8 + 7 = "14.0" (15) -> dist: 1.0000
        8 + 5 = "12.0" (13) -> dist: 1.0000
        2 + 8 = "1.0" (10) -> dist: 9.0000
        10 + 7 = "6.0" (17) -> dist: 11.0000
        8 + 4 = "14.0" (12) -> dist: 2.0000
        7 + 6 = "14.0" (13) -> dist: 1.0000
        5 + 2 = "3.0" (7) -> dist: 4.0000
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

`/`, range(1, 100):
```

Prompt accuracies:                                                                                                                                                       
Prompt 0: 0.00%, Avg answer distance: 8.2022
        70 / 64 = "0.0" (1.09375) -> dist: 1.0938
        61 / 76 = "1.0" (0.8026315789473685) -> dist: 0.1974
        63 / 87 = "0.0" (0.7241379310344828) -> dist: 0.7241
        60 / 41 = "0.0" (1.4634146341463414) -> dist: 1.4634
        34 / 32 = "0.7" (1.0625) -> dist: 0.3625
        72 / 91 = "0.0" (0.7912087912087912) -> dist: 0.7912
        34 / 53 = "0.3" (0.6415094339622641) -> dist: 0.3415
        63 / 75 = "75.0" (0.84) -> dist: 74.1600
        62 / 57 = "0.0" (1.087719298245614) -> dist: 1.0877
        36 / 15 = "0.6" (2.4) -> dist: 1.8000
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

```
Proszę pana posła | przekręcił  pismo  pokoju  panoszy.
Badanie było bolesne | bolały,  bu.d.
Po przesłuchaniu połączonym | przez  płytę  pt  Persons  postanowiłem  posłuchać,  p  pod  piosenką  po  przesłuchaniu  połączonym  p.
Po przyznaniu punktów | procentowych  poszczególne  przedszkola  prezentowały  przygotowany  przez  p.
Pierwszy polski portal  poświęcony  problemom  prawnym  przedstawia  PRAWOOBIE,  PRAWOSZKOLE.
```
