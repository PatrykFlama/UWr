[wróć](..)

# Połączenie problemów NP-trudnych i granic dolnych
* 3SAT
* zbiór niezależny

## Definicja problemu 3SAT
3SAT = $\{\phi : \phi -$formuła logiczna postaci $C_1 \land C_2 \land ... \land C_n$ gdzie $C_i$ jest postaci $x_{i_1} \lor x_{i_2} \lor ...$ gdzie każde $x_{ij}$ - zmienna logiczna lub jej negacja$\}$
![image](image-2.png)

## Definicja problemu zbioru niezależnego
Zbiór niezależny = $\{G = (V, E) : G -$graf nieskierowany, $V -$zbiór wierzchołków, $E -$zbiór krawędzi, $I \subseteq V$ jest zbiorem niezależnym jeśli nie istnieje krawędź $e \in E$ taka, że oba końce $e$ należą do $I\}$

##
3SAT $\in$ NP-zupełny (Łatwo znaleźć świadka - przypisanie wartości zmiennym)  
mamy problem A i B oraz funkcję działającą w czasie wielomianowym przekształcającą dane $f: A \to B$  
więc jeżeli potrafimy rozwiązać problem B w czasie q(n) i f działa w p(n), gdzie n to rozmiar danych, to możemy rozwiązać problem A w czasie $q(n) + p(n)$  

każdy rpoblem NP-zupełny da się zredukować do 3SAT, ale żeby nie dowodzić tego dla każdego problemu wystarczy że pokamżemy:  
3SAT $\le_p$ jest redukowalny do zbioru niezależnego  
A $\in$ NP-zupełny -> 3SAT-> Zbiór niezależny

### Redukcja 3SAT do zbioru niezależnego
jeżeli jesteśmy w stanie wielomianowo zredukować 3SAT do prbolemu A, to problem A jest NP-trudny bo 3SAT jest NP-zupełny  
dla każdej klauzuli tworzymy trókjąt łączący wszystkie wierzchołki, etykietujemy je nazwą zmiennej (rozróżniając negację), między grafami tworzymy krawędzie łączące zmienne z ich negacjami (tak żeby zbiór niezależny nie brał na raz ich obu stanów)
![image](image.png)

### izomofizm problemów np zupełnych
gdyby istniało rozwiązanie P dla problemu NP to istniałaby redukcja z niego do wszystkich innych problemów NP  

## 3SUM
3SUM = ${\{a_1, a_2, ..., a_n\} : \exists i, j, k \in \{1, 2, ..., n\} a_i + a_j + a_k = 0}$
Alg nietrudny: O($n^2$)  
udowodnić się nie da za pomocą zwykłych drzew decyzyjnych (samych porównań), bo jesteśmy w stanie skonstuować takie zestawy liczb, dla których odp jest TAK oraz NIE, a zachowują one relację między sobą czyli drzewo dałoby tą samą odpowiedź (np {-1, 0, 1}, {0, 1, 2})  
d-d liniowe drzewa decyzyjne, redukcja do innego probemu który max O($n^2$)  
![image](image-1.png)