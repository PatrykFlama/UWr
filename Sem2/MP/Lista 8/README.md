[(wróć)](../)

# Lista 8
| 1 | 2 | 3 | 4 | 5 | 6 | 7 |
|---|---|---|---|---|---|---|
| X | X | X |   |   |   |   |


## Zadanie 1
Aby stworzyć cykl parsujemy się (_ys_) po wejściowej tablicy, pamiętając jej początek (_xs_). Gdy dojdziemy do ostatniej pary zamieniamy jej drugi element _null_ na odwołanie do początku tablicy _xs_.

## Zadanie 2
Aby odwrócić tablicę pamiętamy wskaźnik na jej poprzednią parę i rekurencyjnie przechodzimy do ostatniego elementu listy. Następnie zamieniamy odwołania do kolejnego elementu (drugi element pary) na przekazany do listy wskaźnik na poprzednią parę. Wyjątkiem jest pierwsza para, która na koniec ma odwołanie do _null_. Następnie zwracamy odwołanie do ostatniego elementu listy (teraz pierwszego).

## Zadanie 3
Aby stworzyć kolejkę dwustronną potrzebujemy listy dwustronnej, aby przy wyrzucaniu elementu z dowolnej strony móc w czasie stałym odpowiednio przesunąć wskaźnik.Nasza struktura kolejki przechowuje wskaźnik na jej początek oraz koniec.\
Przy dodawaniu elementu wystarczy stworzyć nowy element listy dwukierunkowej, z zadaną wartością, jednym wskaźnikiem ustawionym na skrajny element listy oraz druim wskaźnikiem pustym. Musimy też połączyć pusty wskaźnik skrajnego elementu listy z naszym nowym elementem. Jeżeli nasza kolejka była pusta to musimy też ustawić drugi skrajny wskaźnik naszej kolejki. \
Przy wyrzucaniu elementu musimy zapisać wartość elementu do wypisania oraz ustawić wskaźnik do skrajnego elementu kolejki w następnym elemencie na _null_. Jeżeli kolejka po wyrzuceniu elementu jest pusta musimy ustawić drugi skrajny wskaźnik na _null_.



