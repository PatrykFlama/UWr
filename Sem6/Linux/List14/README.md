# List 14


## Zad 1
1. essential - wymagane do działania, dodatkowo możemy polegać na tym że na pewno one są
2. znaczenie priorytetów
  - required
  - important - podstawowa funkcjonalność systemu się bez nich posypie/będzie ograniczona (np vim/nano)
  - standard - "każdy rozsądny użytkownik spodziewałby się na normalnym systemie użytkowym" (takie standard utilities)
  - optional - tutaj jest większość pakietów (jest tutaj również kernel, bo np lxc)
  - extra

między required a imprtant nie powinno być żadnych konfliktów  
w standard już mogą być 'pakiety których nie chcemy' (np faq)  
3. mamy kanoniczną listę wszystkich sekcji  
4. listy pakietów
  - depends - pakiety potrzebne w czasie runtime
  - predepends - pakiety porzebne zanim zainastalujemy pakiet (wykorzystywane w czasie instalacji)
  - recommends, suggests - różne poziomy rekomendacji
  - breaks - pakiet może zepsuć drugi pakiet, ale sam działać
  - conflicts - pakiet może zepsuć drugi pakiet, i oba nie będą działać
  - replaces - jaki pakiet zastępuje nasz pakiet (spełnia jego funkcjonalność, chodzi o nazwologię)
  - provides - jaki pakiet wirtualny dostarcza nasz pakiet
5. etykiety, o strukturze drzewa, każdy maintainer mógł dodać tyle tagów ile chce (ale w ogólności debtagi są już depreceated)
6. różne stany w jakich może się znaleźć pakiet
  - not-installed
  - config-files
  - half-installed
  - unpacked
  - half-configures
  - trigger-awaited
  - trigger-pending
  - installed
7. `/var/lib/apt` `less extended_states`
8. `/var/lib/aptitude/pkgstates`
9.  `tasksel --list-tasks` - zbiory paczek, do czego chcielibyśmy systemu używać (wyświetla się pod koniec instalacji systemu `--new-install`)
10.  pakiety wirtualne nie istnieją, np x-window-manager - jest to po to, aby móc łatwo twórzyć zbiór pakietów dostarczających tą samą funkcjonalność
11.  .



## Zad 2
https://www.debian.org/doc/manuals/aptitude/

### Podstawowe polecenia
- `aptitude update`
- `aptitude upgrade` lub `safe-upgrade`
- `aptitude full-upgrade` - może ususwać pakiety aby spełniać zależności
- `aptitude {install, remove, purge, reinstall} <pakiet>`
  - `install` - instaluje pakiet(y), można zastosować wzorce do wyszukiwania lub wyspecyfikować wersję
  - `remove` - usuwa pakiet zostawia jego pliki konfiguracyjne
  - `purge` - usuwa pakiet i jego plikii konfiguracyjne
  - `reinstall`
- `aptitude search <wzór>` - wyszukuje pakiety wg wzorca
- `aptitude show <pakiet>` - pokazuje informacje o pakiecie
- `aptitude why <pakiet>` (lub `why-not`) - dlaczego pakiet powinien być / nie może być zainstalowany
  - `why` znajduje łańcuch zależności który instaluje dany pakiet
  - `why-not` znajduje łańcuch zależności powodujący konflikt
- `aptitude versions` - wypisuje wersje pakietów
- `aptitude clean` - czyści cache z plików .deb
- `aptitdue autoclean` - usuwa z cache pakiety, które nie mogą być już zainstalowane
- `aptitdue download` - pobiera .deb danego pakietu

### Podstawowe opcje
- `-y` - assume yes
- `-s` - symulacja (nie wykonuje zmian)
- `-f` - próba naprawienia zależności zepsutych pakietów
- `-D` - pokazuje zależności (od)instalowanych pakietów
- `-v` - verbose
- `-R`/`-r` - nie instaluj / instaluj rekomendowane pakiety

### Interfejs okienkowy (ncurses)
możemy do niego przejść wywołując samą komendę `aptitude`  

- `/` - wyszukiwanie pakietów
- `u` - update
- `g` - preview/download/install/remove
- `+`/`:`/`-`/`_` - zaznacza pakiet do install/reinstall/remove/purge

### Wzorce wyszukiwania
https://www.debian.org/doc/manuals/aptitude/ch02s04s05.en.html#tableSearchTermQuickGuide

- `~nXXX` - pakiety których nazwa zaiwera XXX
- `~dXXX` - to samo dla opisu
- `~i` - zainsatlowane
- `~U` - można zaktualizaować
- `~M` - zainastlowane autoamtycznie
- `~N` - nowe pakiety
- `?obsolete` - 'osierocone' pakiety
- `?and(X,Y)` - spakiety spełniające X i Y


## Zad 3 - instalacja debiana za pomocą dbootstrap



