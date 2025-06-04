# List 14

## Zad 2
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

## Podstawowe opcje
- `-y` - assume yes
- `-s` - symulacja (nie wykonuje zmian)
- `-f` - próba naprawienia zależności zepsutych pakietów
- `-D` - pokazuje zależności (od)instalowanych pakietów
- `-v` - verbose
- `-R`/`-r` - nie instaluj / instaluj rekomendowane pakiety

## Interfejs okienkowy (ncurses)
możemy do niego przejść wywołując samą komendę `aptitude`  

- `/` - wyszukiwanie pakietów
- `u` - update
- `g` - preview/download/install/remove
- `+`/`:`/`-`/`_` - zaznacza pakiet do install/reinstall/remove/purge

## Wzorce wyszukiwania
https://www.debian.org/doc/manuals/aptitude/ch02s04s05.en.html#tableSearchTermQuickGuide

- `~nXXX` - pakiety których nazwa zaiwera XXX
- `~dXXX` - to samo dla opisu
- `~i` - zainsatlowane
- `~U` - można zaktualizaować
- `~M` - zainastlowane autoamtycznie
- `~N` - nowe pakiety
- `?obsolete` - 'osierocone' pakiety
- `?and(X,Y)` - spakiety spełniające X i Y


