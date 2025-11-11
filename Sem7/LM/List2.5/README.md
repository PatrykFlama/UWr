[(back)](../)

# List 2.5
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
|---|---|---|---|---|---|---|---|---|----|
| x |   | x | x | x | x |   | x | x |    |


## Zad 1
Może to działać, bo perplexity to miara zaskoczenia modelu językowego wobec danego tekstu - więc wg intuicji tekst wygenerowany przez model nie powinien być zaskakujący dla innego modelu (podąża za jakimiś schematami).  
Ale nie jest to idealne rozwiązanie, bo ludzie też potrafią pisać w sposób przewidywalny/schematyczny, a modele mogą generować teksty z  większym perplexity (np zwiększając temperaturę generacji, zmieniając top-p/top-k aby rozszerzyć zakres możliwych tokenów do wyboru).

## Zad 2
Taka metoda jest bardzo niewydajna - liczba możliwych niepoprawnych generacji rośnie wykładniczo wraz z długością tekstu (liczbą słów). 



### a)  losuj tekst o długości M, który na pozycji k ma określony wyraz
generowanie prefiksu i suffiksu osobno - będizemy generować prefiks idąc 'do tyłu', co działa dalej na ten samej zasadzie

### b) losuj tekst o długości M, który na pozycjach parzystych ma określone wyrazy (czyli losujesz tylko pozycje nieparzyste, zaczynamy numerację od 0),


## Zad 3
Udało się ten model wytrenować z sukcesem, więc wiemy już że nasze dane są dobrze zpreparowane oraz że uczenie nowego modelu tez zakończy się sukcesem.  
Model może zosatć wykorzystany jako baza do nowego modelu - nauczył się już jakichś struktur języka, więc mieliśmy więcej danych do nauczenia modelu.  
Model jest przydatny do zadań generacji poprzedniego tokenu.  Możliwe jest połączenie obu modeli przy generacji odpowiedzi.  


## Zad 4
<!-- jedno zdanie = jedna litera. Najczęściej występująca litera (w słowach, lub tylko pierwsze litery słów) to litera kodu.  -->

Ustalamy model, ustalamy że dla wygenerowanych prawdopodobieństw tokenów - tokeny na miejscu nieparzystym to zapalony bit, a na parzystym to zgaszony. Wtedy mamy wiadomość gdzie jeden bit kodujemy jednym tokenem. Ideę można też wymienić na 8 bitowe kodowanie w analogiczny sposób.


## Zad 5
> Zasada Kerckhoffsa - system kryptograficzny powinien być bezpieczny nawet wtedy, gdy wszystkie szczegóły jego działania – oprócz klucza – są znane 

Zależy od tajności modelu - bo spełnia jeśli kluczem jest model, a ten bazuje na losowych prawdopodobieństwach (nieznany jest korpus ani sposób nauczenia modelu). Nie sposób więc odgadnąć, jak prawdopodobny był dany token dla naszego modelu.

Jeżeli natomiast model jest publicznie dostępny, to dałoby się odszyfrować wiadomość. Wtedy wystarczy uznać za tajny klucz jakąś losową permutację tokenów.


## Zad 6
Iterujemy się token po tokenie, w zadanym zdaniu. Dla każdego takiego prefiksu zapisujemy, który z kolei token wg papugi (jak bardzo prawdopodobny) został wybrany do zdania. 


## Zad 7
Tokeny papugi zaczynają się od spacji, więc dodanie spacji na końcu słowa nie przejawiało się w korpusie na którym była ona trenowana. Przez to żaden token za bardzo nie pasuje do naszej generacji i model zaczyna się gubić.

bez spacji vs ze spacją:  
```
`Kot wskoczył na`
Kot wskoczył na plecy i stanął na podłodze.
Kot wskoczył na łów. I po raz pierwszy nie jest w stanie ruszyć w stronę z jakiej się nie ruszył.

`Ala ma`
Ala ma już na swoim koncie występy w reprezentacji Polski, a wcześniej występował w kadrze narodowej w latach 2002-2004. W 2006 roku biało-czerwoni zakończyli rozgrywki na trzecim miejscu w grupie. W tym samym roku wywalczyli awans do finałów mistrzostw świata w Niemczech.
Ala ma ��
@lalalalalalalalalalalalalalalalalalalalalalalalalalalalalalalalalal 1 mc i jeszcze 2 mc i na razie nie mam problemu, w zeszlym roku przynioslam z pracy 2 lata temu. W tym roku urodzila sie 2 latka. I nie wiem co robic, bo mam juz dosyc.

`Składniki potrzebne do zrobienia naleśników to`
Składniki potrzebne do zrobienia naleśników to: mąka, jajka, sok z cytryny, przyprawy, olej, sól, czosnek i cynamon.
Składniki potrzebne do zrobienia naleśników to owca ziemniaczana, kukurydza, bułka, jaja, mąka pszenna, sól, olej, śmietana, cukier, zarodki pszenne, jajko, dżem, mleko, mąka razowa, proszek do pieczenia, soda, sól, cukier, olej, cukier waniliowy, olej, jajko, bułka tarta, mąka ziemniaczana, olej, przyprawy, cukier, woda, jaja, mleko, soda, cukier, mąka pszenna, jaja, jogurt, mąka ziemniaczana, olej, śmietana, sól, cukier, mąka ziemniaczana, mąka pszenna, mąka ziemniaczana, mąka pszenna, mąka ziemniaczana, mąka ziemniaczana, cukier, masło, mąka pszenna, mąka ziemniaczana, woda, mleko, mąka ziemniaczana, olej, mleko, masło, woda, cukier, drożdże, mąka pszenna, mąka ziemniaczana, sól, mąka ziemniaczana, sól, sól, cukier, mleko, cukier, mąka ziemniaczana
```


## Zad 8
Cechy wierszów:
- liczba sylab (powstarza się), ustalona liczba wersów
- rymy (np parzyste wersy się rymują)

Algorytm:  
- na początku ustalamy cechy wiersza - liczba wersów i sylab
- dobieramy rymy (całe słowa lub końcówki) dla każdego wiersza - można to zrobić nie wykorzystując jeszcze modelu
- teraz mamy zadanie wygenerowania zdania o zadanej długości (liczbie sylab) oraz końcówce (rym), dodatkowo ma ono kontekst poprzednich wersów - możemy to rozwiązać beam searchem


## Zad 9
propozycje:
- przewidywanie genomu na podstawie fragmentu
- przewidywanie ruchów w grach planszowych (np szachy) na podstawie historii ruchów
- przewidywanie kolejnych kroków w przepisach kulinarnych na podstawie wcześniejszych kroków
- przewidywanie kolejnych akcji użytkownika w aplikacji na podstawie historii akcji
- przewidywanie kolejnych zdarzeń w logach systemowych


np przewidywanie kolejnych kroków w przepisach kulinarnych na podstawie wcześniejszych kroków - korpus bierzemy z przepisów kulinarnych, natomiast tokenizację (żeby nie operować na języku naturalnym) możemy zrobić na podstawie akcji (np. "dodać", "wymieszać", "piec") oraz składników (np. "mąka", "jajka", "cukier")



