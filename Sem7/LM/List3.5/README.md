# List 3.5
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 |
|---|---|---|---|---|---|---|---|---|----|----|
| x |   |   |   |   |   |   |   |   |    |    |


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



