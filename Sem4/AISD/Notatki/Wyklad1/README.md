[(wróć)](../)

# Kopiec
### Definicja
Kopiec to drzewo binarne, które spełnia dwie własności:
* **struktura drzewa** - drzewo binarne jest pełne, tj. wszystkie poziomy drzewa są wypełnione, z wyjątkiem ostatniego, który jest wypełniony od lewej do prawej.
* **uporządkowanie** - dla każdego węzła `v` w kopcu, wartość w `v` jest większa lub równa wartości w każdym z węzłów potomnych.

### Implementacja
Kopiec można trzymać w tablicy. Operacje przejścia z węzła-rodzica o indeksie _k_ do dzieci będzie polegała na $2k$, $2k+1$ (gdzie $2k$ można szybko policzyć za pomocą operacji bitowej $k<<1$). Natomiast aby przejść do dziecka _k_ do rodzica wykonamy dzielenie całkowite $k/2$ ($k>>1$).

### Operacje
#### Przywracanie własności kopca względem jednego elementu
Np po dodaniu jednego elementu, kopiec niekoniecznie nadal będzie spełniać swoje własności. Strukturę drzewa będziemy zachowywać np poprawnie dodając element (lub usuwając etc). Uporządkowanie może się zepsuć zarówno względem wierzchołków-dzieci jak i wierzchołka-rodzica (chociaż ewaluacja zazwyczaj będzie szła w jedną stronę, bez cofania się).  
'Idąc do korzenia' naprawa polega na sprawdzeniu czy nasz wierzchołek względem rodzica i jego drugiego dziecka są poprawnie uporządkowane (np wartość rodzica > dzieci), jeżeli trzeba było jakieś wierzchołki zamienić, to musimy też je naprawić.  
'Idąc do liści' naprawa polega na sprawdzeniu czy nasz wierzchołek względem dzieci jest poprawnie uporządkowany, jeżeli nie to zamieniamy z większym z dzieci i naprawiamy wierzchołek-dziecko.  

#### Budowanie kopca
Dostając tablicę elementów możemy stworzyć kopiec dodając elementy jeden po drugim $O(n\log(n))$. Słaby drań.  
Sortowanie? też $O(n\log(n))$.  
Istnieje rozwiązanie $O(n)$, traktujemy elementy jako ułożony już kopiec i ewaluujemy je od strony liści na każdym poziomie sprawdzając tylko raz czy są poprawnie uporządkowane (jeżeli tak to musimy też je naprawić).

### Zastosoawnia
#### Heapsort
Tworzymy kopiec O(n). Następnie wyciągamy n elementów O(n), po każdym wyciągnięciu elementu naprawiamy kopiec - bierzemy ostatni liść i wstawiamy w miejsce korzenia (struktura drzewa zachowana), po czym naprawiamy $O(\log(n))$. Łącznie mamy czas $O(n\log(n))$.  
Problem - zazwyczaj ostatni liść będzie bardzo małym elementem (wynika to ze struktury kopca), więc naprawa kopca zazwyczaj zajmie $2O(\log(n))$, 
dodatkowo każde porównanie będzie wymagać 2 operacji (rodzic i 2 dzieci). Możemy temu zapobiec nie wynosząc małego elementu na samą górę, a znosząc dziurę na sam dół (teraz 1 operacja porównania bo tylko 2 dzieci) $O(\log(n))$, 
po czym wstawiamy ostatni element z drzewa w miejsce dziury i ponownie go naprawiamy (teoretycznie $2O(\log(n))$, ale w praktyce pójdziemy tylko kilka wierzchołków w górę).

#### Kolejka priorytetowa
Operacja czytania elementu - korzeń $O(1)$.  
Operacja dodawania elementu - wstawiamy jako ostatni i naprawiamy $O(\log(n))$.  
Operacja usuwania elementu - jak w heapsort  $O(\log(n))$.

#### Kolejka priorytetowa dwustronna (podobno useless)
Idea rozwiązania opiera się na dwóch kopcach L (min) i H (max), gdzie łącznie każdy element jest pamiętany dokładnie raz. Kopiec L pamięta $\lfloor{n/2}\rfloor$ elementów, a kopiec H $\lceil{n/2}\rceil$ elementów.  
Kopce są 'sklejone' ze sobą liśćmi (puste liście przeskakujemy), nakładamy na taki sklejony kopiec warunek, że każda ścieżka (naturalnie zdefiniowana jako nie cofająca się w wysokości) między korzeniami jest uporządkowana monotonicznie.  
Dodawanie elementu polega na wybranie kopca, do którego dodajemy oraz dodaniu go do adekwatnego liścia, następnie sprawdzamy wynosimy element w górę kopca, po czym wynosimy element w tym liściu w górę kopca-sąsiada. $O(\log(n))$ 
Aby usunąć wyrzucamy element min/max i wrzucamy na jego miejsce ostatni element tego kopca, jeżeli L i H równoliczne, albo nadmiarowy ostatni element. Następnie naprawiamy kopiec. $O(\log(n))$

___
### Ciekawe zadanie
Niech k << n  
uwtórz algorytm znajdujący k największych elementów (szyyyyybko).  
