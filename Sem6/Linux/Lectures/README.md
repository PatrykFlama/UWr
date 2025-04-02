
- [Some notes](#some-notes)
  - [konwencja](#konwencja)
    - [konwencja wykonywania komend:](#konwencja-wykonywania-komend)
    - [konwencja programu wypisującego do terminala](#konwencja-programu-wypisującego-do-terminala)
    - [konwencja tworzenia plików wykonwyalnych](#konwencja-tworzenia-plików-wykonwyalnych)
  - [stos katalogów w bashu](#stos-katalogów-w-bashu)
  - [tworzenie pliku z ograniczonym odczytem](#tworzenie-pliku-z-ograniczonym-odczytem)
  - [`type` vs `which`](#type-vs-which)
  - [pl\_pl.utf8, en\_US.utf8](#pl_plutf8-en_usutf8)
  - [`$(ls)` vs `"$(ls)"`](#ls-vs-ls)
  - [`(...)` vs `((...))` vs `$((...))`](#-vs--vs-)
  - [globy](#globy)
  - [po co jest `$@`?](#po-co-jest-)
  - [moduły jądra](#moduły-jądra)
- [Wykład 2](#wykład-2)
    - [Operatory sterujące](#operatory-sterujące)
    - [Operatory przekierowania](#operatory-przekierowania)
    - [Słowa kluczowe](#słowa-kluczowe)
- [Wykład 4 - deskryptory plików](#wykład-4---deskryptory-plików)
  - [Składnia przekierowań](#składnia-przekierowań)
  - [Przykłady](#przykłady)
  - [Przykłady przekierowań](#przykłady-przekierowań)
  - [Składnia przekierowań](#składnia-przekierowań-1)
    - [Kolejność](#kolejność)
  - [Dygresja - wypisywanie na terminal a przekierowywanie do pliku](#dygresja---wypisywanie-na-terminal-a-przekierowywanie-do-pliku)
  - [Operacje na plikach](#operacje-na-plikach)
- [Wykład 5 - podstawowe czynności administracyjne](#wykład-5---podstawowe-czynności-administracyjne)
  - [Co to jest plik?](#co-to-jest-plik)
  - [Hierarchia](#hierarchia)


# Some notes
## konwencja
### konwencja wykonywania komend: 
```bash
$ komenda -opcje argumenty
```

### konwencja programu wypisującego do terminala
powinien on się kończyć znakiem nowego wiersza

### konwencja tworzenia plików wykonwyalnych
`skrypt.sh` oczekujemy że nie jest oznaczony jako plik wykonwyalny i będzie odpalany przez `bash skrypt.sh`  
`skrypt` i w środku _she-bang_ oczekujemy że jest oznaczony jako plik wykonwyalny i będzie odpalany przez `./skrypt`

## stos katalogów w bashu
`pushd` - dodaje katalog na stos
`popd` - usuwa katalog ze stosu
`dirs` - wyświetla stos katalogów

(aczkolwiek podobno overkill i `cd -` wystarcza)

## tworzenie pliku z ograniczonym odczytem
normalnie w trakcie tworeznia pliku w folderze publicznym jest on dostępny dla wszystkich, więc może on być otworzony przez kogoś innego i nie zostanie wykopany bo uprawnienia są sprawdzane tylko przy odczynie (zapytania do kernela)  
dlatego biblioteka jest mądrzejsza i tak na prawdę najpierw tworzy katalog, nadaje uprawnienia dostępu tylko dla właściciela, a potem tworzy plik w tym katalogu

## `type` vs `which`
`type` - sprawdza czy komenda jest wbudowana w bashu, aliasem, funkcją, czy programem
`which` - sprawdza gdzie jest program, czyli gdzie jest plik wykonywalny

## pl_pl.utf8, en_US.utf8
język_wariant.utf8  

## `$(ls)` vs `"$(ls)"`
`$(ls)` - wykonuje `ls` i zwraca wynik jako argumenty do komendy  
`"$(ls)"` - wykonuje `ls` i zwraca wynik jako jeden string

## `(...)` vs `((...))` vs `$((...))`
`(...)` - wykonuje komendy w nowym procesie  
`((...))` - wykonuje operacje arytmetyczne
`$((...))` - wykonuje operacje arytmetyczne i zwraca wynik jako argument do komendy (token)

```bash
$> N=5
$> ((N++))
$> echo $N # 6

$> N=5
$> N=$((N+1))
$> echo $N # 6
```

## globy
zazwyczaj mają się dostosować do nazw plików w bieżącym katalogu, od razu się tokenizują (ale w specjalny sposób, tylko 1 raz)   
`*` - zwraca wszystkie pliki w bieżącym katalogu   
`**` - zwraca wszystkie pliki w bieżącym katalogu i podkatalogach
`?`  

## po co jest `$@`?
```bash
#plik ts.sh
ls "$@"

# bash
$> ./ts.sh "a b" /tmp

# rozwinie się do ls "a b" /tmp
```

## moduły jądra
`lsmod` listuje moduły jądra, oraz ich zależności  
`insmod` - wczytuje moduł do jądra  
`modprobe` - dodaje/usuwa moduły z jądra  



# Wykład 2
### Operatory sterujące
Używane do sterowania wykonywaniem poleceń w powłoce  

- `<newline>` – kończy linię poleceń i powoduje ich wykonanie
- `||` – wykonuje drugie polecenie tylko wtedy, gdy pierwsze zakończyło się błędem (OR logiczne)
- `&&` – wykonuje drugie polecenie tylko wtedy, gdy pierwsze zakończyło się sukcesem (AND logiczne)
- `&` – uruchamia polecenie w tle
- `;` – oddziela polecenia, wykonując je kolejno
- `;;` – używane w `case`, kończy blok `case`
- `;&` – powoduje wykonanie kolejnego bloku `case` bez sprawdzania warunku
- `;;&` – wykonuje kolejny blok `case`, sprawdzając jego warunek
- `|` – przekazuje wyjście jednego polecenia do wejścia drugiego (pipe)
- `|&` – przekazuje zarówno standardowe wyjście, jak i błędy do wejścia następnego polecenia
- `(` i `)` – grupowanie poleceń w podpowłoce

---

### Operatory przekierowania
Pozwalają na przekierowanie wejścia/wyjścia

- `<` – przekierowanie wejścia z pliku
- `>` – przekierowanie wyjścia do pliku (nadpisuje)
- `<<` – tzw "here document", pozwala wprowadzać wieloliniowe wejście
- `>>` – przekierowanie wyjścia do pliku (dopisywanie)
- `<<<` – tzw "here string", przekazuje podany ciąg jako wejście
- `<>` – otwiera plik do jednoczesnego odczytu i zapisu
- `&>` – przekierowuje zarówno standardowe wyjście, jak i błędy do pliku
- `>&` – alternatywna forma przekierowania wyjścia standardowego do innego deskryptora
- `<&` – przekierowanie wejścia z innego deskryptora
- `>&` – przekierowanie wyjścia do innego deskryptora
- `&>>` – przekierowuje standardowe wyjście i błędy do pliku (dopisywanie)

---

### Słowa kluczowe
Zarezerwowane słowa języka powłoki, używane w instrukcjach sterujących

- `!` – negacja warunku
- `case` – instrukcja warunkowa podobna do `switch`
- `coproc` – tworzy współproces
- `do` – używane w pętlach `for`, `while`, `until` do rozpoczęcia bloku kodu
- `done` – kończy pętlę `for`, `while`, `until`
- `elif` – `else if` w `if`
- `else` – alternatywna ścieżka w `if`
- `esac` – kończy `case`
- `fi` – kończy `if`
- `for` – pętla iteracyjna
- `function` – deklaracja funkcji
- `if` – instrukcja warunkowa
- `in` – używane w `for` oraz `case`
- `select` – tworzy menu wyboru
- `then` – rozpoczyna blok `if`
- `until` – pętla wykonywana, dopóki warunek jest fałszywy
- `while` – pętla wykonywana, dopóki warunek jest prawdziwy
- `{` i `}` – grupowanie poleceń w bloku
- `time` – mierzy czas wykonania polecenia
- `[[` i `]]` – rozszerzona składnia dla warunków logicznych



# Wykład 4 - deskryptory plików
**UWAGA niektóre spacje na slajdach są niepoprawne: `n >&-` nie zadziała, powinno być `n>&-`**

___

w jądrze znajduje się tablica (tak na prawdę to to nie jest tablicja, jest to zoptymalizowane, ale tak łatwiej o tym myśleć), która indeksuje otwarte pliki

informacje o nich znajdują się w pseudofolderze `/proc` (symulowanym przez kernel)  

gdy dany proces prosi o pseudofolder `/proc/self` to rozwija się on do `/proc/<pid>` gdzie `<pid>` to identyfikator tego procesu  
więc:
* `/proc/<pid>/fd` - folder zawierający deskryptory plików procesu o identyfikatorze `<pid>`  
* `/proc/self/fd` - folder zawierający deskryptory plików procesu, który wywołuje polecenie

## Składnia przekierowań
* `d> plik`  - nadpisanie pliku
* `d>> plik` - dopisanie do pliku
* `d>| plik` - nadpisanie pliku, nawet jeśli jest on zablokowany (przez `set -o noclobber`)
* `d>&n`    - przekierowanie deskryptora `d` na deskryptor `n`
* `d<& -`   - zamknięcie deskryptora `d`
* `d>&-`    - zamknięcie deskryptora `d`

(brak podanego `d` oznacza deskryptor `1`)

## Przykłady

przykład:
```bash
ls -l /proc/self/fd
```
rozwinie się do numeru procesu `ls` a nie naszego procesu `bash` w którym został on wykonany:  
```txt
... 0 -> /dev/pts/0
... 1 -> /dev/pts/0
... 2 -> /dev/pts/0
... 3 -> /proc/12345/fd
```

> skąd się wzięło 3?  
* 0 - stdin  
* 1 - stdout
* 2 - stderr
* 3 - deskryptor pliku `/proc/12345/fd` (folder z deskryptorami plików procesu o identyfikatorze `12345`) - został mu przypisany pierwszy wolny deskryptor

> `$$` - pid procesu (powłoki uruchomionej jako program) oraz wszystkich procesów potomnych uruchomionych przez tę powłokę jako program
> `$BASHPID` - pid naszej powłoki
> `$PPID` - parent pid

inny przykład:
```bash
$ cat script.bash
ls -l /proc/$$/fd
$ bash script.bash
```

```txt
... 0 -> /dev/pts/0
... 1 -> /dev/pts/0
... 2 -> /dev/pts/0
... 255 -> /proc/12345/fd
```

> skąd się wzięło 255?
zostało otwarte przez basha, więc żeby się nie mieszało z otawrtymi przez użytkownika deskryptorami, został przypisany jakiś duży deskryptor


_____


```bash
$ ulimit -n
65535
$ exec 65545> file1
$ ls -l /proc/$$/fd
```
```txt
...
... 65545 -> /home/user/tmp/file1
```

`exec` - pozwala na wykonanie polecenia w obecnym procesie, ale z zamknięciem wszystkich deskryptorów plików, które nie są związane z terminalami


____

```bash
$ exec 65536> file2
bash: exec: 65536: Bad file descriptor
$ ls -l /proc/$$/fd
```
```txt
...
... 3 -> /home/user/tmp/file2
```

zdążymy otworzyć plik, ale nie uda się przypisać mu deskryptora, bo jest on poza zakresem

<details>
    <summary>Jakaś dygresja</summary>

```py
>>> x=([],[])
>>> x[0]+=[1]
krotka nie jest modyfikowalna
>>> x
([1], [])
```

</details>


## Przykłady przekierowań
* `echo Message >&2 2>/dev/null`  
było:    
1 -> /dev/pts/   
2 -> /dev/pts/   

po wykonaniu:  
1 -> /dev/pts/   
2 -> /dev/null  

* `echo Message 2>/dev/null 1>&2`   
po wykonaniu:  
2 -> /dev/null  

a potem:  
1 -> 2 <=> 1 -> /dev/null  

* `echo Message 2>&1- 2>/dev/null`  
po wykonaniu:  
2 -> /dev/pts  
i zamykamy deskryptory (dostęp do terminala)  

## Składnia przekierowań
### Kolejność
przekierowania mogą wystąpić w dowolnym miejscu instrukcji prostej, przykład:

```bash
$ < /dev/random tr -cd 'A-Z' | head -c 24 | fmt > pwd xtx
$ tr -cd 'A-Z' < /dev/random | head -c 24 | fmt > pwd xtx
```

po instrukcji złożonej (nie przed)  
```bash
while grep ... > /dev/null
```

## Dygresja - wypisywanie na terminal a przekierowywanie do pliku
jak wspominaliśmy mamy barierę _userland_ od _jądro_, przez którą komunikujemy się **syscallami** (np dostęp do pliku w jądrze za pomocą `write(2)`),   
ale jest to kosztowne więc wolimy buforować dane w _userlandzie_ i przekazywać je do jądra w jednym kawałku  
ale jest to niewygodne więc istnieją rozwiązania robiące takie bufory za nas (np funkcje z biblioteki libc `fwrite(3)` `fprintf(3)`)  
(w efekcie powstaje wiele warstw takich buforów, żeby było to wydajne)

np `fwrite(3)` zależnie od tego gdzie pisze, buforuje w różny sposób (output do terminala - linia po linii, output na stderr - od razu)

## Operacje na plikach
`open file object (OFO)` - struktura w jądrze, która przechowuje informacje o pliku (np offset, tryb otwarcia, ...)
`vnode` - struktura w jądrze, która przechowuje informacje o pliku (np inode, ...) 


Mamy 2 syscalle: `open` i `close`  
`open` tworzy nowy deskryptor, tworzy open file object (może być współdzielony przez wiele deskryptorów) oraz vnode (chyba że już istnieje vnode)

`close` zamyka deskryptor, ale nie musi zamykać open file object ani vnode (bo inne deskryptory mogą na nie jeszcze wskazywać)  


jak to się może stać że jakieś deskryptory współdzielą OFO?  
`dup` - duplikuje deskryptor (tworzy nowy deskryptor, który wskazuje na ten sam OFO)
`dup2` - duplikuje deskryptor (tworzy nowy deskryptor, który wskazuje na ten sam OFO) ale na podany deskryptor

_______
**KONIEC OMAWIANIA POWŁOKI SYSTEMOWEJ**
_______

# Wykład 5 - podstawowe czynności administracyjne
pomysł na unixa: wszystko jest plikiem i wszystko żyje w systemie plików  

## Co to jest plik?
dawniej dane przechowywano na kartach perforowanych, później przeszło to w bębny magnetyczne (w formie cashe'u), dyski, blah blah blah  

> zbiór wielu kart (stos) nazywany był plikiem, pliki kart były organizowane za pomocą folderów  

w systemie wszystko jest wirtualizowane (procesor, ram, ...), więc powstał pomysł wirtualizacji dysku -> system plików  
jądro nie powinno/musi wiedzieć jak interpretować zawartość plików, to zadanie należy do userlandu  


aby zidentyfikować dyski przypisujemy im 2 liczby: majory i minory (które dla łatwości interpretacji przez użytkownika są reprezentowane jako np `/dev/sda`), kiedyś `/dev/` było faktycznym folderem, teraz jest to symulowane przez jądro  
są rozwiązania gdzie osobne dyski żyją w osobnych drzewach, co wiele upraszcza bo istnieją różne systemy plików, w unixie przez to że jest tylko jedno drzewo plików dołożona została dodatkowa warstwa abstrakcji wyrównująca różnice między systemami plików (w kernelu)  

systemy plików ciągle się rozwija, np system `ext2` dostał w superbloku dodatkowe miejsce na oznaczenie rozszerzenia plików  

dla poprawienia niezawodności stworzony został `journal` - dziennik, który przechowuje zmiany w systemie plików, aby w razie awarii można było je odtworzyć

wprowadzenie dysków typu flash było problematyczne, bo wszystkie systemy były dostosowane do dysków magnetycznych (np lokalność przestrzenna)  

pseudosystemy plików:  
- `sysfs` - pliki hardware'owe, informacje o systemie i urządzeniach
- `procfs` - pliki procesów
- `udevfs` - pliki urządzeń
- `tempfs` - pliki tymczasowe (zapisywane tylko w ramie)  
(dołożona więc została dodatkowa warstwa abstrakcji - `vfs`)

procesy mogą się komunikować za pomocą:  
- plików
- pipe'ów (jedno kierunkowe)
- socketów (dwu kierunkowe)
- inter process communication (ipc) - katalog w któym trzyma się uchwyty do stron pamięci, w której procesy mogą się komunikować

## Hierarchia
- w `rootfs` mamy 2 linki symboliczne: jądro i initramfs  
- `/etc/` - pliki konfiguracyjne systemu
- `/bin/` `/sbin` - pliki binarne dla użytkowników i administratorów
- `/lib*/` - biblioteki systemowe (współdzielone)
- `/root/` `/home/` `/usr/home` - katalogi domowe użytkowników, katalog roota jest w innym miejscy, tak aby mógł być na innej partycji (home roota jest potrzebny do uruchomienia systemu, katalogi innych użytkowników mogą być dołożone później)  
- `/proc/` - procfs
- `/sys/` - sysfs
- `/dev/` - udevfs
- `/tmp/` `/run/` - tempfs

- `/var/` - zmienne pliki (np logi, cache, ...)
- `/usr/` - drugi poziom hierarchii, osobny punkt montażowy na systemy plików (może być domontowany później, ale nie zawsze np w nowym debianie)  
- `/opt/` - oprogramowanie opcjonalne
- `/srv/` - dane lokalne serwera (np apache, ftp, ...)
- `/media/` `/mnt/` - montowanie systemów plików 

