[(wróć)](../)

# Zachłany
Konceptem zachłana jest proste i szybkie rozwiązanie problemu, które niekoniecznie jest poprawne (oceniamy stopień zbliżenia do optymalnego rozwiązania). Zachłanne algorytmy są proste w implementacji i zazwyczaj działają w szybkim czasie. Dlatego też skupiać się będziemy na analizie ich poprawności (kiedy dają poprawne wyniki i jak daleko są od optymalnych).  
Przykład - problem wydawania reszty, rozwiązywany wybierając największe noimnały.  

## Konstrukcja MST
Dowody algorytmów _zostawiamy jako ćwiczenie dla czytającego_ (były na dyskretnej)  
* Kruksal
sortujemy krawędzie rosnąco i dodajemy je do drzewa, jeśli nie tworzą cyklu
* Prim-Dijkstra
wybieramy wierzchołek startowy, a następnie dodajemy do drzewa krawędzie o najmniejszej wadze, które łączą drzewo z pozostałymi wierzchołkami
* Boruvka
dla każdego wierzchołka wybieramy najkrótszą krawędź, następnie łączymy spójne składowe w jeden wierzchołek i powtarzamy kroki do póki nie zostanie jeden wierzchołek  
uwaga na szczególne przypadki, np. grafy z krawędziami o tej samej wadze  

## Szeregowanie zadań
#### Problem
Mamy _n_ zadań z czasem wykonywania $t_i$, procesor jest jednowątkowy, chcemy zminimalizować sumę czasów zakończenia zadań. Zachłan, ez, teraz dowód **NW**:  
**teza:** gdy uporządkujemy zadania rosnąco względem czasu to uzyskamy najlepszy czas  
weźmy permutację zadań $\pi = (i_1, i_2, ..., i_n)$ i widzimy że koszt wynosi  
$$T(\pi)=\sum_{k=1}^{n} (n-k+1) t_{i_k}$$  
załóżmy **NW** że $\exist \pi$ tż $\pi$ jest najleszą permutacją (z najlepszym czasem) oraz że istnieje $\pi'$ taka że dla pewnych $x < y, t_{i_x} > t_{i_y}$ o koszcie  
$$T(\pi')=(n-x+1)t_{i_y} (n-y+1) t_{i_x} + \sum_{k=1, k\neq xy}^{n} (n-k+1) t_{i_k}$$
wtedy
$$T(\pi)-T(\pi')=(n-x+1) t_{i_x} + (n-y+1) t_{i_y} - (n-x+1)t_{i_y} - (n-y+1) t_{i_x} = $$
$$ = t{i_x} (n-x+1 - (n-y+1)) + t_{i_y} (n-y+1 - (n-x+1)) = (y-x)(t_{i_x}-t_{i_y}) > 0$$
co prowadzi do sprzeczności bo znaleźliśy permutację z mniejszym kosztem od $\pi$.

<!-- TODO szeregowanie z terminami ++ -->

## Problem pokrycia zbioru
Mamy zbiór podzbiorów zbioru _X_ gdzie każdy podzbiór ma nadany koszt. Chcemy wybrać zbiór podzbiorów o minimalnym koszcie, tak aby ich suma była równa _X_.

### Strategie zachłanne
* wybieranie podzbiorów o najmniejszym koszcie
* wybieranie podzbiorów o największej liczbie elementów
* wybieranie podzbiorów o najmniejszym koszcie na element
* wybieranie podzbiorów o najmniejszym koszcie na niewybrane elementy

### Analiza strategii (4)
<!-- TODO -->

