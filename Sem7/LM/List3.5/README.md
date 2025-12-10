# List 3.5
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10*| 11*|
|---|---|---|---|---|---|---|---|---|----|----|
| x | x | x |   | x | x |   |   |   |    |    |


## Zad 1
tonie:  
- jako tonąć, topić się
- jako toń, głębina
- jako tona, waga
- jako ton, np głosu

Tonie skrywają skarby, tonie kapitan statku, który w tonie spokoju spekuluje o tonie złota.


> W zdaniu *"Tonie skrywają skarby, tonie kapitan statku, który w tonie spokoju spekuluje o tonie złota."* słowo **"tonie"** pojawia się trzy razy, ale w każdym przypadku ma inne znaczenie, wynikające z kontekstu. Oto ich wyjaśnienie:  
> 1. **Tonie skrywają skarby** — tutaj *tonie* jest formą czasownika "tonąć", czyli "zatonąć". Całe wyrażenie oznacza, że skarby są ukryte w morskich głębinach, gdzie statki lub przedmioty toną. Można to rozumieć jako metaforę dla skarbów znajdujących się w zatopionych wrakach statków, na dnie morza.
> 2. **Tonie kapitan statku** — w tym przypadku *tonie* również jest formą czasownika "tonąć", ale odnosi się do samego kapitana, co sugeruje, że kapitan statku (może dosłownie lub metaforycznie) znajduje się w trudnej sytuacji, być może w niebezpieczeństwie, na przykład zatonął jego statek lub sam ma problemy.
> 3. **Tonie spokoju** — tutaj *tonie* jest rzeczownikiem i oznacza "ton" w sensie nastroju lub stylu. "Tonie spokoju" oznacza, że w rozmowie, postawie lub atmosferze panuje spokojny, stonowany nastrój, a kapitan jest wyciszony i zrównoważony.
> 4. **Tonie złota** — w tym przypadku *tonie* jest także rzeczownikiem i odnosi się do jednostki miary wagi, czyli "tony". "Tonie złota" oznacza dużą ilość złota, ważącą dokładnie tonę.
>
> Podsumowując, w tym zdaniu słowo "tonie" występuje w różnych formach gramatycznych i ma różne znaczenia: czasownik "tonąć", rzeczownik "ton" (w sensie nastroju) i rzeczownik "ton" (jednostka miary).


>> W ostrym tonie kapitana, gdy przeciążony jedną tonie ładunku statek tonie, w ciemne tonie oceanu spadają ostatnie skrzynie.

## Zad 2
Rzadkie reprezentacje to wektory kontekstów o wielu zerach (tudzież niewielkiej gęstości informacji) - dobrze pokazują współwystępowania, lecz rozdzielają formy leksykalne i mieszają znaczenia. Na przykład "piękna żaglówka" i "śliczny żaglowiec" mogą być sensownie bliskie, a w TF‑IDF traktowane oddzielnie (będą odległe).  
Dodatkowo, jak w zad 1, słowa wieloznaczne są mieszane w jednej reprezentacji.

Koncept algorytmu:  
Będziemy chcieli połączyć słowa o podobnym znaczeniu - przeprowadzimy więc klasteryzację wektorów kontekstów (cosinusowe podobieństwo).  
Następnie przypiszemy każdemu słowu etykietę klastra, do którego należy jego wektor kontekstów.  
Możemy zastosować klasteryzację K-means.


## Zad 3
a) przykłady negatywne losowane z całego korpusu  
b) przykłady negatywne to sąsiadujące zdania w odwróconej kolejności  


- w (b) możemy porównać wiarygodności obu permutacji - i wybrać najlepszą
- możemy potraktować papugę jako enkoder całego tekstu (np bierzemy reprezentację ostatniego tokena po przepuszczeniu przez papugę) - na podstawie tego enkodera wytrenować klasyfikator binarny
- w wariancie (a), aby zdeterminować czy zdania mogły wystąpić koło siebie, możemy policzyć TF-IDF obu zdań i sprawdzić ich podobieństwo cosinusowe


## Zad 4

## Zad 5
Będziemy polegać na korpusie przysłów - będziemy chcieli znaleźć pasujące do pytania przysłowie oraz podać je do prompta modelu. 
Aby znaleźć pasujące przysłowie, możemy skorzystać z osadzeń kontekstów (z papugi, lub TF-IDF) i znaleźć najbliższe przysłowie do pytania (np  podobieństwo cosinusowe).  


## Zad 6
The WIKI_SEARCH baseline solution uses the question as a query to Wikipedia search service and returns the title of the first returned article as an answer, as long as it doesn’t overlap with the question.

Specifically, the following procedure is used:

1. Split the question into tokens using spaCy (model pl_core_news_sm) and ignore the one-character tokens,
2. Send the space-separated tokens as a query to the Search API of the Polish Wikipedia,
3. For each of the returned articles:
   1. Split its title into tokens with spaCy,
   2. If none of the tokens of the title has at least 50% overlap (measured as in Evaluation) with any of the tokens of the question:
      1. remove the part of the title starting from ‘(‘, if found
      2. return the title as an answer,
   3. Otherwise, continue to the next result,
4. If no answer is found at this point, remove the first of the question tokens and jump back to (2).


Dlaczego może to działać?  
- pytania dotyczą wiedzy ogólnej/faktów - Wikipedia jest dobrym źródłem takich informacji
- silnik wyszukiwania Wikipedii jest zoptymalizowany pod kątem znajdowania artykułów odpowiadających zapytaniom użytkowników
- unikamy artykułów, które dotyczą bardziej samego pytania, niż odpowiedzi

Sensowna korekta:
- wybieramy kilka pasujących odpowiedzi i każdą testujemy modelem językowym (wybieramy najbardziej prawdopodobną)
- zamiast ignorować jednowyrazowe tokeny, możemy ignorować `stop words` - aby zapytanie było bardziej informacyjne


