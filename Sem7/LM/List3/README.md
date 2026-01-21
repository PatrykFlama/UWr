Ocena zadania z odpowiadaniem na pytania będzie przeprowadzona w następujący sposób (potrzebne dane znajdują się w tym katalogu):

- można w dowolny sposób korzystać z plików questions_train.txt i answers_train.txt (2000 pytań)
- system powinien przygotować plik tekstowy found_answers.txt zawierający odpowiedzi (po jednej w każdym wierszu)
- ocenie podlega wynik systemu dla części testowej (questions_test.txt i answers_test.txt) -- 500 pytań.
- ocenę przeprowadza skrypt check_answers.py. Nie wolno analizować wyników części testowej inaczej niż za pomocą tego skryptu
- należy koniecznie przygotować 500 odpowiedzi (oczywiście, jeżeli procedura licząca będzie bardzo wolna, to można część odpowiedzi obliczyć prostszym programem, w szczególności programem piszącym napis "nie wiem") 
- każdy użyty model powinien mieć 2B*32bity parametrów. Oznacza to, że można korzystać z modeli skompresowanych, na przykład przy 4bitach / parametr parametrów może być 16B. Można używać więcej niż jednego modelu, sumaryczna wielkość użytych modeli nie jest istotna.
- Punktacja jest następująca: dzielimy procenty prawidłowych odpowiedzi przez 10, odejmujemy 1 i ta liczba oznacza punkty za to zadanie.
- Programy studenckie i (być może) programy prowadzących zostaną ustawione w rankingu (wspólnym dla wszystkich grup). Każda osoba dostaje dodatkowo (5 * 0.8 **pozycja) punktów (pozycja liczy się od zera).
