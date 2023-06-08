[(back)](../)

# List 5
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11| 12|
|---|---|---|---|---|---|---|---|---|---|---|---|
|   |   |   |   |   |   |   |   |   |   |   |   |


## Dodatkowe zadanie do P5
### Kwadratura kwadratu
Mamy 24 kwadraty o bokach 1, 2, 3, ..., 24. Należy napisać program, który rozmieszcza je na kwadratowej planszy o boku 70, w ten sposób, aby były spełnione następujące warunki:

1. współrzędne lewego górnego rogu każdego kwadratu są liczbami całkowitymi
2. każdy kwadrat w całości mieści się na planszy
3. kwadraty nie nachodzą na siebie (ale mogą się stykać, rogami lub bokami)
4. boki rozmieszczanych kwadratów są równoległe do odpowiednich boków planszy
5. możliwie jak najmniejsza powierzchnia planszy rostaje niepokryta kwadratami

Twój program powinien wypisywać proponowane rozwiązanie w następującym formacie:

1. w pierwszym wierszu powinna być wypisana liczba niezapełnionych kwadracików
2. po niej powinno być 70 wierszy po 70 znaków przedstawiających rozmieszczenie kwadratów
3. każdy znak jest kropką (oznaczającą brak kwadratu) lub literą 'ABCDEFGHIJKLMNOPQRSTUVWX' ('A' oznacza przynależność do kwadratu o boku 1, 'B' -- o boku 2, itd)

Punktacja za zadanie jest następująca (punkty się sumują):

* 2p -- za rozwiązanie zgodne ze specyfikacją
* 1p -- za 1000 lub mniej wolnych pól,
* 1p -- za 800 lub mniej wolnych pól, czas działania programu kilkanaście sekund
* 2 * 0.5 -- jeżeli Twój program wykorzystuje 1 lub 2 idee/algorytmy z wykładu (i potrafisz je wskazać)

Dodatkowo dostajesz max(0, (200 - W) / 100), gdzie W jest liczbą wolnych pól. Dla uzyskania tego bonusu możesz pozwolić programowi działać dłużej (ale na tyle krótko, byś mógł go uruchomić przy prowadzącym pracownię)


## Propozycje projektów
### Projekt PR-1
Zaproponuj i rozwiąż wersję zadania z autkami, w którym:

mamy możliwie dobrą wierny model fizyczny (im więcej "float", tym lepiej)
agent uczy się dla różnych torów, natomiast ostateczny test przeprowadza na torze wcześniej niewidzianym (można założyć, że dostępna jest "mapa" tego toru o niskiej rozdzielczości, i że można wykonać paraminutowy preprocessing tej mapy)

(raczej trudne zadanie)

### Projekt PR-2
Zaproponuj i rozwiąż jakieś zadanie związane z uczeniem ze wzmocnieniem, w którym częścią świata jest dwuwymiarowe fizyka zaimplementowana w bibliotece Box2D (lub innej wybranej przez Ciebie).

### Projekt PR-3 
Napisz program, który gra w grę karcianą, o poziomie komplikacji tysiąca (czyli ma licytację i rozgrywkę). Program powinien wykorzystywać A-B Search lub MCTS, powinien również modelować jakoś możliwe rozkłady kart u przeciwników (oczywiście musi grać "uczciwie", czyli nie może korzystać z wiedzy o zakrytych kartach przeciwników).

### Projekt PR-4 
Zaimplementuj agentów dla gry "Pędzące żółwie", opisaną np. na Portalu Matematycznym: http://www.matematyka.wroc.pl/grystrategiczneilogiczne/pedzace-zolwie W Twojej implementacji powinieneś zarządzać jakoś hipotezami odnośnie koloru żółwia innych graczy. Zanalizuj, jak przykładowi agenci radzą sobio w rozgrywkach przeciwko sobie (w wersji z więcej niż dwoma graczami).

### Projekt PR-5
Fuga barokowa jest utworem, w którym temat (jakaś niezbyt długa, w miarę ładna, melodia) jest przekształcana na różne sposoby (obracana, podwyższana, przyśpieszana, zwalniana, i pewnie wiele innych), i następnie te przekształcone utwory są puszczane jednocześnie, z jakimś przesunięciem. Jednocześnie brzmiące dźwięki powinny tworzyć ładne współbrzmienia (czyli należeć do katalogu dozwolonych akordów). Napisz program, który zadanie tworzenia fugi traktuje jako zadanie optymalizacyjne, w którym celem jest utwór możliwie gęsty (dużo sytuacj, w których jednocześnie gra wiele głosów), najlepiej bez niedozwolonych współbrzmień. Zaprezentuj kilka kompozycji, bazujących na Twoich ulubionych prostych melodiach (może bajki dla dzieci? Disco-polo?).

### Projekt PR-6 
Kompozytor kanonów. Kanon jest krótką melodią (na przykład 8 taktów), która jest puszczana kilka razy z przesunięciem (na przykład 2 taktów). I kolejne przesunięte wobec siebie kawałki tego kanonu ładnie brzmią (czyli dźwięki się nie powtarzaja i "nie gryzą ze sobą"). Napisz program, który korzystając z bazy melodii (na przykład w formacie abc) uczy się generować podobne melodie, do tych z bazy, które mogą być puszczane jako kanony. 

### Projekt PR-7
Wybierz dwie łamigłówki (Obrazki logiczne i jeszcze jakaś), których rozwiązanie zakodujesz za pomocą zmiennych logicznych. Przeprowadź eksperymenty, w których różne instancje tych łamigłówek są rozwiązywane zgodnie z następującą strategią: przekształcamy łamigłówkę do formuły logicznej (logika zdaniowa, CNF), którą rozwiązujemy specjalistycznym narzędziem (wybierz więcej niż 1 SAT-solver, kierując się wynikami konkursów, opiniami użytkowników, dostępnoścą, etc).

### Projekt PR-8
Przeanalizuj kilka różnych agentów grających w grę Oszust (z listy C4).

### Projekt PR-9
Rozważamy następujący wariant gry w Dżunglę, w którym gracz A ma jednego pionka (w tej roli mogą wystąpić różne zwierzęta), gracz B -- wszystkie, ustawione na początku w sposób standardowy. Rozważamy dwa sposoby gry dla gracza B:

ruchy w pełni losowe (tworzymy listę ruchów i wybieramy 1)
wybieramy losowo ruch w stronę jamy przeciwnkika, a jeżeli takich nie ma -- po prostu ruch losowy.
Dla każdego z tych wariantów (i wybranych zwierząt w roli jedynego pionka gracza A) opracuj sposób wykorzystania metod uczenia ze wzmocnieniem w celu stworzenia agenta skutecznie grającego w taką grę.

### Projekt PR-10
Analiza możliwości uczenia się heurystyk do A*, dla wybranego problemu.

### Projekt PR-11
Przeanalizuj możliwość zastosowania algorytmów ewolucyjnych dla tworzenia funkcji heurystycznej oceniającej planszę w wybranej grze (najlepiej Reversi lub Dżungla, żeby można było porównać z innymi agentami, ale możliwe są też inne wybory)

### Projekt PR-12
Potraktuj wybraną (raczej trudną) łamigłówkę jako grę (dla jednego gracza) i pokaż, jak ją rozwiązywać za pomocą algorytmy MCTS

### Projekt PR-13
[PaPuGaPT-2](https://huggingface.co/flax-community/papuGaPT2) jest polskim wariantem GPT-2 (przodka/starszego brata ChatGPT), który może sensownie działać na pojedynczym, w miarę zwykłym komputerze. Umie on generować teksty (w miarę po polsku i w miarę sensowne), dla zadanego prefiksu (dokładnie tak, jak ChatGPT). W tym projekcie chodzi o to, by połączyć jego działanie z jakimś wariantem beam-search, aby generowane teksty były bardziej "poetyckie". Przy czym utwór poetyckie można sobie zdefiniować dość dowolnie, ale w taki sposób, by łatwo sprawdzać, co jest, a co nie jest poezją (ważne: ta definicja musi być niezależna od używanego modelu, nie można powiedzieć, że poezja to to, co wygeneruje GPT). Przykładowa definicja poezji:

Czterowiersz o 8 sylabach w każdym wierszu, ze schematem rymów abcb, możliwie podobny rytmicznie do:

W Pacanowie kozy kują,
więc koziołek, mądra głowa,
błąka się po całym świecie,
aby dojść do Pacanowa

Pisanie poezji jest pracą twórczą, więc nie wymagamy, żeby tego typu uwtór generował się jakoś bardzo szybko (godzinka jest ok). To zadanie ma potencjał na pracę inżynierską lub wyżej, ale coś powinno się dać zrobić z budżetem 'projektowym'.

### Projekt PR-14
Model językowy (taki jak ChatGPT) umie generować sekwencje symboli (na przykład słów). Ale umie też generować sekwencje innych rzeczy, przykładowo ruchów w danej grze (o ile zobaczy wystarczająco wiele przykładów gier). W tym projekcie, który bardzo łatwo można rozwinąć do pracy inżynierskiej, zajmiemy się grą Przełomy (https://en.wikipedia.org/wiki/Breakthrough_(board_game)). Należy znaleźć zbiór 10K-100K rozgrywek (o możliwie wysokiej jakości) i za pomocą tego zbioru wytrenować wersję GPT (na przykład nanoGPT, co wymaga zaskakująco mało przeróbek, generator Shakespeare'a prawie od razu może działać jako agent grający w grę). Być może część zbioru uczącego powinna być wygenerowana za pomocą utworzonego przez Studenta/Studentkę programu, na przykład agenta MCTS. W zadaniu należy zbadać jakość gry GPT-agenta. Można też wybrać inną grę, o podobnej komplikacji.


