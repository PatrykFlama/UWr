# List 2


## Task  1
Wnioski do modelu _papugaGPT_ (wybranie mocniejszego modelu, takiego jak polka, **powinno** dać lepsze wyniki):  
Dla równań prostych - składających sie tylko z 2 wartości oraz prostych operatorów (dodawanie i odejmownie) i małych liczb - model jest w stanie kilkukrotniej dokładniej przybliżyć wynik niż losowe zgadywanie, a czasem nawet trafić w poprawną odpowiedź. Jednak po dołożeniu mnożenia lub odejmowania, papuga bardzo szybko (wraz ze wzrostem wartości liczb) traci jakąkolwiek trafność, a nawet daje gorsze przybliżenia niż losowe zgadywanie.   

Przetestowane prompty:
```py
("Oblicz wartość wyrażenia: ", "\nWynik:"),
("Wartość wyrażenia matematycznego ", " to"),
("\nPodaj wynik działania: ", "\nWynik to:"),
("Calculate the result of the expression: ", "\nResult:"),
("", " ="),
("Student rozwiązuje równanie ", " jego wynik to"),
("Studentka rozwiązuje równanie ", " jej wynik to"),
```
Najwydajniejszy prompt dla tego modelu to *"Wartość wyrażenia matematycznego XXXX, to YYYY"*


`+`, range(1, 10):
```
Prompt accuracies:                                                                                                                                                       
Prompt 0: 10.00%, Avg answer distance: 9.8000

Prompt accuracies:                                                                                                                                                       
Prompt 0: 5.00%, Avg answer distance: 6.5000
```

`+, -`, range(1, 1000):
```
Prompt 0: 0.00%, Avg answer distance: 495.2000 (expected random ~1500)
```

`*`, range(1, 10):
```
Prompt accuracies:                                                      
Prompt 0: 0.00%, Avg answer distance: 45.2000 (expected random ~50)
```

`/`, range(1, 100):
```
Prompt accuracies:                                                                                                                                                       
Prompt 0: 0.00%, Avg answer distance: 8.2022
```


```py
("Student rozwiązuje równanie ", " jego wynik to"),
("Studentka rozwiązuje równanie ", " jej wynik to"),

Prompt accuracies:                                                                                                                                                                                    
Prompt 0: 0.00%, Avg answer distance: 281.6200
Prompt 1: 0.00%, Avg answer distance: 122.6800
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
Pierwszy polski portal | poświęcony  problemom  prawnym  przedstawia  PRAWOOBIE,  PRAWOSZKOLE.
```
