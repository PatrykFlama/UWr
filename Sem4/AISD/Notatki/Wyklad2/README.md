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
<!-- TODO -->
na wykładzie poleciał skip

## Problem pokrycia zbioru
Mamy zbiór podzbiorów zbioru _X_ gdzie każdy podzbiór ma nadany koszt. Chcemy wybrać zbiór podzbiorów o minimalnym koszcie, tak aby ich suma była równa _X_.

### Strategie zachłanne
* wybieranie podzbiorów o najmniejszym koszcie
* wybieranie podzbiorów o największej liczbie elementów
* wybieranie podzbiorów o najmniejszym koszcie na element
* wybieranie podzbiorów o najmniejszym koszcie na niewybrane elementy

### Analiza strategii (4)
<!-- TODO -->

