
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
- [Wykład 6 - systemy plików (cd)](#wykład-6---systemy-plików-cd)
  - [mount, fstab](#mount-fstab)
  - [identyfikatory i nazwy urządzeń](#identyfikatory-i-nazwy-urządzeń)
  - [urządzenia znakowe w katalogu `/dev/`](#urządzenia-znakowe-w-katalogu-dev)
  - [urządzenia blokowe w katalogu `/dev/`](#urządzenia-blokowe-w-katalogu-dev)
  - [chroot](#chroot)
  - [loopback](#loopback)
  - [szyfrowanie](#szyfrowanie)
  - [access control](#access-control)
- [Wykład 7 - podstawowe czynności administracyjne](#wykład-7---podstawowe-czynności-administracyjne)
  - [Name Service Switch](#name-service-switch)
  - [użytkownicy i grupy](#użytkownicy-i-grupy)
    - [/etc/shadow](#etcshadow)
    - [grupy](#grupy)
    - [podużytkownicy i podgrupy](#podużytkownicy-i-podgrupy)
    - [jak zmienić zapomniane hasło roota?](#jak-zmienić-zapomniane-hasło-roota)
    - [zarządzanie użytkownikami](#zarządzanie-użytkownikami)
    - [hasło roota](#hasło-roota)
  - [logi systemowe](#logi-systemowe)
- [Wykład 8 - daemony, BSD init i SystemD](#wykład-8---daemony-bsd-init-i-systemd)
  - [tldr końcówka poprzedniego wykładu](#tldr-końcówka-poprzedniego-wykładu)
  - [init - proces o numerze 1](#init---proces-o-numerze-1)
  - [BSD init](#bsd-init)
  - [dygresja: metodologie programowania](#dygresja-metodologie-programowania)
  - [SystemD](#systemd)
    - [Targety](#targety)
    - [jednoski systemd](#jednoski-systemd)
    - [przykłady](#przykłady-1)
      - [`fstrim`](#fstrim)
      - [interfejsy sieciowe](#interfejsy-sieciowe)
      - [`/tmp` w ramdysku](#tmp-w-ramdysku)
    - [krytyka](#krytyka)
- [Wykład 9 - rootfs](#wykład-9---rootfs)
  - [Kernel boot time parameters](#kernel-boot-time-parameters)
  - [Jak się montuje systemy plików?](#jak-się-montuje-systemy-plików)
    - [ramdysk](#ramdysk)
    - [buforowanie urządzeń blokowych (dygresja)](#buforowanie-urządzeń-blokowych-dygresja)
    - [współczesne rozwiązanie początkowego systemu plików](#współczesne-rozwiązanie-początkowego-systemu-plików)
  - [Systemy działające tylko w ram](#systemy-działające-tylko-w-ram)
  - [eksperyment](#eksperyment)
  - [initrd we współczesnych systemach](#initrd-we-współczesnych-systemach)
  - [minimalistyczny initramfs](#minimalistyczny-initramfs)
  - [initramfs-tools](#initramfs-tools)
  - [dracut](#dracut)
- [Wykład 10 - środowisko uruchomieniowe procesorów x86-64 (Legacy BIOS)](#wykład-10---środowisko-uruchomieniowe-procesorów-x86-64-legacy-bios)
  - [1. **Wstęp: BIOS i jego rola**](#1-wstęp-bios-i-jego-rola)
  - [2. **Procesor 8088/8086**](#2-procesor-80888086)
  - [3. **Rozruch systemu**](#3-rozruch-systemu)
  - [4. **GRUB (Grand Unified Bootloader)**](#4-grub-grand-unified-bootloader)
  - [5. **Tryby pracy procesora x86-64**](#5-tryby-pracy-procesora-x86-64)
  - [6. **Proces przełączania trybów**](#6-proces-przełączania-trybów)
  - [7. **Problemy Legacy BIOS**](#7-problemy-legacy-bios)
  - [8. **Podsumowanie**](#8-podsumowanie)
- [Wykład 11 - środowisko uruchomieniowe procesorów x86-64 (UEFI. Bootloadery)](#wykład-11---środowisko-uruchomieniowe-procesorów-x86-64-uefi-bootloadery)
  - [1. **UEFI vs Legacy BIOS**](#1-uefi-vs-legacy-bios)
  - [2. **GUID Partition Table (GPT)**](#2-guid-partition-table-gpt)
  - [3. **Secure Boot**](#3-secure-boot)
  - [4. **Problemy z implementacją UEFI**](#4-problemy-z-implementacją-uefi)
  - [5. **Bootloadery w ekosystemie Linuksa**](#5-bootloadery-w-ekosystemie-linuksa)
  - [6. **Konfiguracja GRUB-a w Debianie**](#6-konfiguracja-grub-a-w-debianie)
  - [7. **Podsumowanie**](#7-podsumowanie)
- [Wykład 12 - Wirtualizacja](#wykład-12---wirtualizacja)
  - [Symulacja](#symulacja)
  - [Emulacja](#emulacja)
  - [Emulacja vs wirtualizacja](#emulacja-vs-wirtualizacja)
  - [Jak zwirtualizować procesor?](#jak-zwirtualizować-procesor)
  - [Pierwszy hypervisor](#pierwszy-hypervisor)
  - [Parawirtualizacja](#parawirtualizacja)
  - [Najpopularniejsze hypervisory](#najpopularniejsze-hypervisory)
  - [chroot(2) i chroot(8)](#chroot2-i-chroot8)
  - [Kontenery - bezpieczniejszy chroot](#kontenery---bezpieczniejszy-chroot)
  - [Namespace i cgroups](#namespace-i-cgroups)
  - [Sandboxing vs kontenery](#sandboxing-vs-kontenery)
    - [Zwykłe vs nieuprzywilejowane konenery (LXC)](#zwykłe-vs-nieuprzywilejowane-konenery-lxc)


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


# Wykład 6 - systemy plików (cd)
system plików składa się z wielu części
- namespace - potrzebujemy mechanizmu nazywania plików oraz katalogów
- access control - atrybuty
- api - w jaki sposób programy użytkownika mogą korzystać z systemu plików
- implementacja


## mount, fstab
z racji iż mamy jedno drzewo plików (rootfs) to musimy mieć mechanizm montowania systemów plików w inne miejsca  
ta warstwa abstrakcji musi umieć 'zmyślać' - np fat32 nie ma atrybutów  

```bash
mount -t <typ> -o <opcje> <źródło> <cel>
```  
> obserwacja: tak wygląda to też w fstabie (te 4 kolumny)

konwencja folderów `XXX.d` - folder w którym na każdy obiekt jest jeden osobny plik konfiguracyjny (np `fstab.d` - folder w którym na każdy system plików jest jeden osobny plik konfiguracyjny)  


**opcje:**
- `defaults` - 'nic', potrzebne żeby dało się w fstab zaznaczyć brak opcji (nie zozstawiając pustej kolumny)
- `ro` - read only, ale uwaga na journal (który mimo tej opcji będzie zapisywać do dysku)  
- `discard` - return zero after trim
- `noatime` - nie aktualizuj czasu dostępu do pliku (przydatne w systemach plików flash)
- `umask` `fmask` `dmask` - maski uprawnień (umask - maska domyślna, fmask - maska dla plików, dmask - maska dla katalogów)


## identyfikatory i nazwy urządzeń
**urządzenia blokowe**  
możemy skoczyć do dowolnego miejsca (seek)

**urządzenia znakowe**  
dostęp opraty na strumieniu (nie możemy skoczyć do dowolnego miejsca, musimy przejść przez wszystkie bajty)

**numer urządzenia nadawany przez jądro**
- majory - klasa urządzenia
- minory - idnyfikator urządzenia w klasie (zależy od kolejności wykrywania urządzeń przez jądro)

**plik urządzenia w katalogu `/dev/`**
zamiast majorów i minorów (które mogą się zmieniać przy każdym uruchomieniu systemu) mamy nazwy urządzeń (które też w jakichś sytuacjach mogą się zmieniać), więc mamy jeszcze uuid przypisane do urządzenia (które się nie zmienia)

## urządzenia znakowe w katalogu `/dev/`
- `ttyn` - terminale wirtualne (symulowany przez jądro na karcie graficznej)
- `tty` - termnial sterujący (dla każdego procesu)
- `ttySn` - terminale szeregowe (np porty COM, RS232, USB)
- pseudoterminale - zawsze wystąpują w parach (master i slave), wykorzystuje się je do komunikacji między procesami (np `ssh`), lub do emulatorów terminali w środowisku graficznym  


- pseudourzadzenia:
  - `full` - zawsze zapełniony
  - `null` - zawsze pusty
  - `zero` - zawsze zapełniony zerami
  - `random` `urandom` - losowe bajty (z cache'u, lub z /dev/random)
  - `kmsg` - kernel message

## urządzenia blokowe w katalogu `/dev/`
namespace w nvme - zwykle jest tylko 1  
urządzenia loopbackowe są symulowane przez kernel  

> historia o nagrywarkach i odtwarzaczach  
> nie opłacało się osobno robić odtwarzaczy, więc były to tak na prawdę nagrywarki, które identyfikowały się w sata jako odtawrzacz (więc windows krzyczał że nie da się nagrywać, ale linux bez problemu mógł nagrywać)

gdy padnie bateria rtc to system przy starcie ustawia czas na czas odmontowania rootfs, żeby zachować ciągłość  
za pomocą `adjusttime` można zapobiegać dryfowi czasu   


## chroot
mamy zapisane *current working directory* oraz *root working directory* w struktórze procesu, więc możemy zmieniać root directory (ale nie current working directory); z konceptu root nie może przejść wyżej niż jego wrokign directory

dodatkowo musismy stworzyć osobno pseudosystemy proc, sys, dev (np za pomocą mount rbind)  
proc i sys możemy stworzyć osobne (nowe) `mount -t proc <cokolwiek - urządzenie blokowe, którego nie ma więc zosatnie zignorowane> /target/proc`, ale dev musi być współdzieloony `mount -o bind /dev /target/dev`  


## loopback
urządzenie blokowe, które symuluje urządzenie blokowe (np plik)  
`losetup` - tworzy urządznie blokowe z obrazu z pliku  
np montowanie obrazu iso
```bash
mount $(losetup -f --show plyta.iso) /mnt
# lub krócej:
mount -o loop plyta.iso /mnt
```

## szyfrowanie 
jacyś mądrzy ludzie wymyślili że jednym kluczem można zaszyfrować max 500MB danych, potem klucz ulega zmęczeniu; liczenie klucza trwa długo, wiec w jądrze jest zaimplementowane drzewo czerwono czarne pamiętające policzone już klucze  
pote stwierdzono że jednym kluczem można zaszyfrować maxx 500GB danych, co by oznaczało max 32 klucze (16TB)  
koniec końców w implementacji jądra jest bug: generowane są 32 klucze, ale referencja jest tylko do pierwszego wiec cały system jest sztfrowany jednym kluczem  

## access control
- mandatory - scentralizowana, prawa przydziela właściciel
- discretionary - rozproszona, obiekty mają właścicieli, którzy nimi zarządzają

w uniksie korzysta się z obu metod (ale głównie z discretionary)   

**tryb dostępu i typ pliku**  
4 najstarsze bity to typ pliku (czyli czy jest to katalog, plik, socket, ...)   
pozostałe 12 dzieli się na 4 grupy po 3 bity (szczególne tryby dostępu oraz tryb dostępu dla właściciela, grupy i innych)


# Wykład 7 - podstawowe czynności administracyjne
komputery wolą liczby, ludzie nazwy symboliczne => jest masa odwzorowań które przypisują liczby do nazw symbolicznych  
np root to użytkownik z id 0  

## Name Service Switch
NSS - system, który pozwala na odwzorowanie nazw symbolicznych na liczby (i odwrotnie)  
czerpie informacje z różnych źródeł (np plików, LDAP, DNS, ...)  

> czemu /etc/passwd musi być dostępny dla każdego (skoro logowanie ma dostęp do roota)?  
> wiele programów chce sprawdzić nazwę użytkownika, więc muszą mieć dostęp do /etc/passwd  
> (dlatego też hasła są trzymane w osobym pliku /etc/shadow)  


## użytkownicy i grupy
logi były trzymane w `/var/log` (ale zostały przejęte przez systemd *journalem*)  
`id` `whoami` - sprawdzają id użytkownika  
`w` `who` - sprawdzają kto jest zalogowany  

mechanizm `group` pomaga zarządzać uprawnieniami (przypisanie do odpowiedniej grupy nadaje jakieś uprawnienia)  
np grupa `adm` - grupa administracyjna; `video` - dostęp do akceleraji graficznej (są pliki odpowiednich urządzeń w `/dev/` które mają tą grupę przypisaną)  

kiedyś każdy daemon miał użytkownika `root`, ale to było niebezpieczne, więc dodano użytkownika `nobody` (o ostatnim numerze), przez to jednak że każdy miał tego użytkownika to był to bardzo _silny_ użytkonik, więc teraz każdy daemon ma swojego użytkonika (zaczynącego się od `_` aby łatwo rozróżnić)  

### /etc/shadow
pole hasła: konwencja jest taka że jeżeli dodamy `!` lub `*` to hasło jest zablokowane (ale nie usunięte)	 
stara rekomendacja ubikey: hasło powinno się zmieniać często; nowa rekomendacja: jeżeli już zmieniać to nie częściej niż raz na rok (bo odchodzi się od uwierzytelniania hasłem)  

### grupy
2 sposoby aby użytkownik skorzystał z grupy: 
* musi do niej należeć (w /etc/group)
* musi znać do niej hasło (w /etc/gshadow)

> np jeżeli chcemy żeby firefox miał dostęp do internetu ale libreoffice nie:  
> moglibyśmy napisać regułkę do firewalla  
> możemy założyć specjalną grupę net i w firewallu dać tylko jej dostęp do internetu  
> jest polecenie set-group, wtedy możemy odpalić firefox w net, a libreoffice bez  
> wada: wszystkie pobierane pliki były w grupie net

### podużytkownicy i podgrupy
`/etc/{subuid,subgid}` - pliki w których są zapisane podużytkownicy i podgrupy

### jak zmienić zapomniane hasło roota?
w przeciwieństwie do dostępu z internetu, ten kto ma fizyczny dostęp do maszyny to może zrobić z nią praktycznie wszystko (co jest dobre)  

trzeba się wchrootować do systemu ratowanego i zmienić hasło roota (bo passwd nie pyta o hasło jeżeli to jest root)  
można też po prostu usunąć hasło z `/etc/shadow`  

> wg ustawy "nie ulega karze ten kto hakuje w zbożnym celu"  
> też wg ustawy "podlega karze ten kto ma na swoim komputerze narzędzia do hakowania" (np wg jakiejś interpretacji przeglądarka)  

są mechanizmy że to trochę utrudnić - np zahasłowany BIOS (hasło można zdjąć kompletnie odcinając zasilanie), GRUB, dysk  

### zarządzanie użytkownikami
usunięcie hasła użytkonikowi to nie to samo co zablokowanie użytkownika  
żeby zablokować użytkownika możemy np usunąć mu shella (wtedy nadal możemy się na niego przełączyć, bo będziemy korzystać z własnego shella)  
można też zablokować go ustawiając mu datę wygaśnięcia na 0 sekundę (czyli 1-01-1970), co jest preferowaną metodą

`sudo su` - nope, lepiej `sudo -i` żeby mieć interaktywną powłokę  
`su -` - zmienia środowisko użytkownika, bardzo ważne szczególnie przy przełączaniu się na roota (bo inaczej zostaje środowisko nierootowego użytkownika, w którym ktoś mógł coś namieszać)  

różne ograniczenia dostępu do konta:
- zablokowany - nie można uruchomić procesu z jego UID
- domyślna powłoka zablokowana
- domyślna powłoka ograniczona
- hasło zablokowane


sudo  
`prog1 | prog1 > root_file` - gdzie dać sudo? `prog1 | prog2 | sudo tee root_file`

> "jak wolisz su od sudo, to czy wolisz capslock'a od shift'a?"  
> ~ hehehaha

### hasło roota
* czy wyłączać?
w sumie jak się wie co robi, to nie trzeba

* czy powinno być takie samo jak użytkownika? 
zakładając że hasło jest silne, jedynym ryzykiem jest fakt, że hasło użytkownika się wypisuje o wiele częściej


## logi systemowe
katalog `/var/log/`, zazwyczaj dba o nie daemon `(r)syslog`   
często serwer logów jest osobny i dobrze odizolowany (żeby nie można było go zhackować)  
narzędzie `ccze` pozwala na kolorowanie logów  

aczkolwiek systemd po wprowadzeniu `journalctl` kompletnie to zmienił (i często syslog nawet nie jest defaultowo instalowany)  
opiera się on na bazie danych (a nie plikach tekstowych), więc do użycia wymaga on `journalctl`  
warto więc się nauczyć korzysatnia z niego, bo jest on o wiele potężniejszy od sysloga  

kernel się odpala tak jak program, więc możemy mu ustawić jakieś argumenty  
np `quiet` - nie pokazuje logów na ekranie w trakcie startu systemu (bo i tak wyświetlają się tak szybko że nie jesteśmy w stanie ich przeczytać)  



# Wykład 8 - daemony, BSD init i SystemD
> czym się różnią dystrybucje linuxa od siebie?  
> - managerem pakietów  
> - systemem init - duży zbiór programów i daemonów zarządających startem i zamykaniem systemu  
> - kernelem, ale tego nie widać


> twi poleca debiana (bardzo rozbudowany i porządnie zrobiony)  
> ma fajnego managera pakietów aptitude  

## tldr końcówka poprzedniego wykładu
- uruchamianie i zatrzymywanie serwisów
polecenie `service` - uruchamia i zatrzymuje serwisy, dodatkowo działa wszędzie

- jak się zdaemonizować?
jest strona manuala `daemon(7)`, ale w systemd tak się nie robi  


## init - proces o numerze 1
init musi być od samego początku działania  
wszystkie inne procesy (o numerze większym niż 1) pojawia się forkiem (np z inita), ale init nie ma od kogo się forkować, więc jest tworzony przez kernel  
sam init nie tworzy systemu, ale uruchamia inne procesy, które tworzą system  
po stworzeniu systemu, init staje się "domem nieżywego dziecka" - zajmuje się sprzątaniem nieżywych dzieci  (jest **ripperem** / żniwiarzem)    

w systemd każdy użytkownik ma swój własny `init` (`systemd.user`)  

kernel nie pozwala na wysyłanie sygnału do inita, którego on nie obsługuje (żeby np default action go nie zabił)  


## BSD init
- `/etc/rc` (run command) - będzie po kolei uruchamiał wszystkie skrypty  
- `/etc/rc.shutdown` - uruchamiany przy zamykaniu systemu  
- `/etc/rc.d/service` - po jednym na serwis

`rcorder(8)` czyta skrypty i z ich specjalnych komentarzy odczytuje kolejność uruchamiania  
wymaga to odczytu oraz posortowania tych skryptów, co spowalnia start systemu  

zalety: bardzo prosty i łatwy do ogarnięcia (każdy skrypt jest zazwyczaj całkiem krótki i łatwy do przeczytania/zrozumienia)   
wady: sekwencyjny, powolny i brak opieki nad postawionymi procesami  

## dygresja: metodologie programowania
programowanie imperatywne (polecenie-nakaz) vs deklaratywne (co chcemy osiągnąć, nie jak)  
problem z programowaniem imperatywnym - jest gdzieś ukryty niejawny stan, ludzie często mają problem z wyobrażeniem soie tego stanu  

to właśnie dało pomysł na systemd - zamiast pisać skrypty, które są imperatywne, napiszmy pliki konfiguracyjne, które będą deklaratywne  

## SystemD
wszystkie pliki konfiguracyjne mają tą samą składnię (ini)   

katalogi z konfiguacjami są w `/{lib,run,etc}/systemd/system/` (uruchamiane w kolejności `lib` -> `run` -> `etc`)  

> obrazek z warstwami; chcieli włożyć `dbus` do kernela (ale jeszcze tego nie zrobili)

składnia plików konfiguracyjnych:
- w kwadaatowych nawiasach mamy sekcje/rozdziały (np `[Unit]`)
- w każdej sekcji mamy opisane jak dany daemon uruchamiać

warto sobie obejrzeć `systemctl list-dependencies`  
`systemctl show` jest mniej ciekawym poleceniem od `systemctl cat` (szczególnie że `show` pokaże informacje o danym procesie, nawet jeżeli on nie istnieje)  

`systemctl`:  
- enable/disable - uruchamia/wyłącza uruchamianie dany daemon przy starcie systemu
- start/stop - uruchamia/zatrzymuje dany daemon
- mask/unmask - zamaskowana usługa się nie odpali (nawet jakby była taka porzeba ze strony innej usługi) 

### Targety
jest jeden `/lib/systemd/system/default.target` - domyślny target, który jest uruchamiany przy starcie systemu  
służą do grupowania usług (np `multi-user.target` - uruchamia wszystkie usługi związane z użytkownikami)  

### jednoski systemd
`X.service` opisuje serwis (przeważnie demon), zastępuje `/etc/rc.d/` z bsd  
zastępują one wszystkie inne jednostki konfiguracyjne systemu  
- `X.netdev` - konfiguracja interfejsów wirtualnych (zastępuje `/etc/network/interfaces`)

### przykłady
#### `fstrim`
`fstrim.timer` - uruchamia fstrim.service co tydzień  
`fstrim.service` - uruchamia fstrim na wszystkich zamontowanych systemach plików, które to obsługują (`ExecStart=/sbin/fstrim -av`)  

ten serwis jest uruchamiany przez timer, który uruchamia go co tydzień - dlatego musimy dopisać mu timers.target  
(takie wymaganie mają wszystkie usługi korzystające z [Timer])  

> cron vs anacron - anacron uruchamia zadania, które nie zostały uruchomione w danym czasie (np jak komputer był wyłączony)

#### interfejsy sieciowe
zmieniona została konwencja nazywnictwa, przy starcie systemu przychodzi systemd i zmienia nazwy interfejsów sieciowych (np na `enp0s3` p - pci, 0 - pci bus, s - slot, 3 - numer slotu)   

przykład konfiguracji zwiększający bezpieczństwo (przydziela losowy mac przy każdym starcie):  
```ini
; eth0.link

[Link]
MacAddressPolicy=random
NamePolicy=kernel database onboard slot path mac
```


```ini
; 99-default.link

[Link]
NamePolicy=kernel database onboard slot path mac
MACAddressPolicy=persistent
```

#### `/tmp` w ramdysku
normalnie jest on jednak trzymany na dysku, ale można przenieść do tmpfs kofigurując `tmp.service` ([Mount] Type=tmpfs)   


### krytyka
konceptem na unixa było zbudowanie go z małych klocków, ale czasem jednak warto zrobić jakiś większy moduł (np `zfs` lub właśnie `systemd`) - fajnie bo wszystko jest dobrze dograne   
z wad jest on duży, a nie rozwiązuje problemu którego nie dałoby się rozwiązać małymi klockami  
w przypadku systemd nie ma replacementów - nie mamy możliwości wymiany jeżeli coś nam sie nie podoba  
przez to że jest to aż tak duży system, to nie ma wielu alternatyw tego całego systemu  
natomias sam systemd narzuca już wszystko pozostałe, więc pozostaje nam niewiele wyboru w ogólności  


# Wykład 9 - rootfs
.....


bootloader uruchamia jądro jako swoją aplikację, więc po jego uruchomieniu jądro prosi bootloader o ..., tryb chroniony bootloader się wyłącza i jądro zaczyna pracować w trybie nadzorczym  
odpala się scheduler -> podstawowe wątki jądra -> init  
ale do tego potrzebny jest rootfs i tu pojawia się problem - żeby go zamontować potrzebujemy dostępu do samego rootfs   
rozwiązanie: initramfs - system plików w ramdysku, który jest montowany jako rootfs, a potem zamontowany jest rootfs  
(chociaż freebsd ma na tyle zaawansowany bootloader że potrafi sobie poradzić bez initramfs)  

## Kernel boot time parameters
[omówienie opcji ze slajdów]  

freebsd potrafi w bezpieczny sposób przekazać hasło z bootloadera do jądra, a w linuxie nie da się tego zrobić bezpiecznie (więc np initramfs musi jeszcze raz pytać o hasło)  

rootfs na początku montuje się jako read-only, np bo chcemy jeszcze puścić fsck przed uruchomieniem systemu. potem system jest przemontowywany (`mount -no remount,rw`) jako read-write  
jądro na początku nie ma terminala do którego mogłoby pisać, więc zapisuje to do swojego bufora, który później jest np odczytywany przez journal (flaga `-b`)  
wartość `init=path`, jeżeli dojdzie do uruchomienia `/bin/sh` to zostaniemy poinformowany, a zamontowany zostanie shell jako root  

## Jak się montuje systemy plików?
mamy listę `file_systems` zamontowanych systemów plików (która nigdy nie może być pusta), na koniec uruchamiania powinien tam się znaleźć rootfs  

### ramdysk
jeżeli chcemy zamontować root za pomocą `root=/dev/sda1` to w jądrze musiałby się znaleźć sterownik sata/nvme  
do tego mamy `initrd` - initial root disk  

urządzenie blokowe tworzone i przechowywane w ramie, kiedyś nie miało nawet wsparcia trimowania (zajmowało tyle miejsca w ramie, ile mu się kazało, nawet jeżeli było puste)  
zwykle wybierany jest jak najprostrzy system plików (np ext2), dodatkowo skompresowany    

wtedy w initrd mamy `/linuxrc` którego zadaniem jest zamontowanie rootfs, a następnie wchrootowanie się do niego. 
dzięki temu system widzi rootfs jako `/` (swój prawdziwy root), ale nadal zostają 'śmieci' w ramie po starym roocie. 
do tego mamy polecenie pivot_root które zamienia rooty miejscami. potem chcemy wyrzucić stary root z pamięci

**i tak się robiło (montowało system) kiedyś**


### buforowanie urządzeń blokowych (dygresja)  
aby przyspieszyć korzystanie z pamięci jest ona buforowana w ramie, 
są też typy pamięci, np swap, które nigdy mają się nie synchronizować z fizycznym dyskiem - to się nazywa ramfs.  
w efekcie tego powstał system plików tmpfs jest to urządzenie które znajduje się tylko w ramie (ale zużywa tylko tyle miejsca ile potrzebuje)  

### współczesne rozwiązanie początkowego systemu plików
> .tar też jest systemem plików (read-only)  

nowy flow: zrobimy rootfs jako tmpfs -> zrobimy z tego archiwum   
archiwum z początku miało być TAR, ale okazało się że jest zbyt dużo jego wersji (dodatkowo jego format jest duży i skomplikowany), więc zdecydowano się na format `cpio`, dodatkowo z kompresją (`gzip`, `xz`, ...)  

teraz zamiast `pivot_root` mamy `switch_root`  


## Systemy działające tylko w ram
w zasadzie to nie musimy montować systemu plików, możemy po prostu odpalić programy z ramdysku  
np programy z livecd, które są ładowane do ramdysku i uruchamiane z niego  


## eksperyment
`unshare -m` - uruchamia nowy proces w nowym namespace, ale nie zmienia mount namespace  
po `pivot_root` odpalamy `chroot .` bo pivot root nie zmienia current working directory  

## initrd we współczesnych systemach
mamy system ssh w intitramfs, po co? np wake-on-lan - karta sieciowa odbiera ramki nawet gdy komputer jest wyłączony, jeżeli odbierze ona odpowiednią ramkę to wysyła do zasilacza sygnał, żeby się uruchomił.
ale jeżeli komputer ma zaszyfrowany dysk, to się nie uruchomi, zatrzyma się na initramfs, więc możemy się do niego połączyć i odblokować dysk (ale jest to niebezpieczne, nie powinno się trzymać odpalonego initramfs, chyba że z secure boot)  

> bardzo pouczające jest rozpakować swojego initramfs i zobaczyć co tam jest (`zcat /boot/initrd.img | cpio -i`) 


## minimalistyczny initramfs
tak na prawdę /proc i /sys były tylko po to potrzebne, żeby /dev/sda zamontować

## initramfs-tools
każdy feature składa się z hooka i ze skryptu (każdy w dedykowanym katalogu)   
pomiędzy `local-block` i `local-premount` montowany jest rootfs

**przykład ze zmianą czcionki**
- wystarczą 2 skrypty
- skrypt będzie ustawiał czcionkę w trakcie uruchamiania initramfs
- hook będzie  kopiował czcionkę do odpowiedniego initramfs  


## dracut
dracut to alternatywa dla initramfs-tools, ma ogromną ilość modułów które można włączyć/wyłączyć  
możemy dodawać swoje nowe moduły (które nie muszą być używane przy kompilacji jądra)  
potrafi on zrobić `switch_root` w drugą stronę, żeby elegancko odmontować i zamknąć system (w debianie do tej pory przy zamykaniu systemd mówi że nie może odmontować rootfs)  


# Wykład 10 - środowisko uruchomieniowe procesorów x86-64 (Legacy BIOS)
<!-- notes made by: deepseek (based on lecture) -->

## 1. **Wstęp: BIOS i jego rola**
- **BIOS** (*Basic Input/Output System*):
  - Firmware zapisany w pamięci ROM na płycie głównej.
  - Udostępnia warstwę abstrakcji sprzętowej (HAL) dla systemu operacyjnego.
  - Zawiera procedury POST (*Power-On Self Test*), inicjalizuje urządzenia, ładuje bootloader z MBR.

- **Proces uruchamiania**:
  - Procesor zaczyna wykonywać kod z adresu `0xFFFF0` (BIOS).
  - BIOS ładuje pierwszy sektor dysku (MBR, 512 B) do pamięci `0x7C00` i przekazuje sterowanie.

---

## 2. **Procesor 8088/8086**
- **Architektura**:
  - 16-bitowe rejestry ogólnego przeznaczenia (AX, BX, CX, DX).
  - Rejestry segmentowe (CS, DS, SS, ES) i adresowanie: `adres = (seg << 4) + offset`.
  - Przestrzeń adresowa: 20-bitowa (1 MiB), podzielona na:
    - **Conventional memory**: 640 KiB (RAM).
    - **Upper Memory Blocks (UMB)**: m.in. pamięć karty graficznej i BIOS (ROM).

- **Ograniczenie pamięci**:
  - "640 KiB wystarczy każdemu" – konwencjonalna pamięć ograniczona ze względu na mapowanie urządzeń w UMB.

---

## 3. **Rozruch systemu**
- **Master Boot Record (MBR)**:
  - Pierwszy sektor dysku (512 B).
  - Struktura:
    - **446 B**: kod bootloadera.
    - **4 x 16 B**: wpisy partycji.
    - **2 B**: sygnatura `0x55AA`.

- **Partycjonowanie**:
  - Typy partycji (np. `0x83` – Linux, `0xEE` – GPT).
  - Partycja rozszerzona (typ `0x0F`) pozwala na więcej niż 4 partycje logiczne.

- **Przykładowy MBR z GRUB2**:
  - Kod ładujący jądro GRUB-a z wolnego obszaru między MBR a pierwszą partycją.
  - Wymaga wyrównania partycji (np. 2048 sektorów dla 1 MiB).

---

## 4. **GRUB (Grand Unified Bootloader)**
- **Fazy działania**:
  1. **Faza 1**: Kod w MBR ładuje dodatkowe moduły.
  2. **Faza 1.5**: Dostęp do systemu plików (np. ext4).
  3. **Faza 2**: Pełne menu rozruchowe z pliku `grub.cfg`.

- **Konfiguracja**:
  - Plik `grub.cfg` definiuje menu i polecenia uruchamiające systemy.
  - Moduły (np. sterowniki systemów plików) dodawane do jądra GRUB-a.

---

## 5. **Tryby pracy procesora x86-64**
- **Real Mode**:
  - 16-bitowy tryb rzeczywisty (domyślny po starcie).
  - Brak ochrony pamięci, bezpośredni dostęp do sprzętu.

- **Protected Mode**:
  - 32-bitowy tryb chroniony z segmentacją i stronicowaniem.
  - Wymaga konfiguracji GDT (*Global Descriptor Table*).

- **Long Mode**:
  - 64-bitowy tryb z rozszerzonymi rejestrami i większą przestrzenią adresową.
  - Wymaga włączenia PAE (*Physical Address Extension*) i PML4.

---

## 6. **Proces przełączania trybów**
- **Włączanie Protected Mode**:
  1. Wyłącz przerwania (`cli`).
  2. Włącz linię A20 (dostęp do pamięci powyżej 1 MiB).
  3. Załaduj GDT.
  4. Ustaw bit PE w rejestrze CR0.

- **Włączanie Long Mode**:
  1. Wyłącz stronicowanie.
  2. Włącz PAE (CR4).
  3. Załaduj PML4 (CR3).
  4. Ustaw EFER.LME (MSR `0xC0000080`).
  5. Włącz stronicowanie.

---

## 7. **Problemy Legacy BIOS**
- **Ograniczenia**:
  - MBR obsługuje tylko 4 partycje (rozszerzone są niewygodne).
  - 32-bitowe LBA ogranicza dyski do 2 TiB.
  - Brak wsparcia dla nowoczesnych standardów (np. NVMe, UEFI).

- **Rozwiązania**:
  - **GPT** (GUID Partition Table) – zastępuje MBR, obsługuje dyski >2 TiB.
  - **UEFI** – nowszy firmware z graficznym interfejsem i wsparciem bezpiecznego rozruchu.

---

## 8. **Podsumowanie**
- **BIOS** to przestarzały, ale wciąż używany standard rozruchu.
- **GRUB** działa jako pośrednik między BIOS a jądrem systemu.
- Współczesne procesory x86-64 zachowują kompatybilność wstecz z trybem rzeczywistym.
- Przejście na UEFI/GPT eliminuje wiele ograniczeń BIOS/MBR.


# Wykład 11 - środowisko uruchomieniowe procesorów x86-64 (UEFI. Bootloadery)
<!-- notes made by: deepseek (based on lecture) -->


## 1. **UEFI vs Legacy BIOS**
- **BIOS** (przestarzały):
  - Ograniczenia: MBR (4 partycje, dyski do 2 TiB), tryb 16-bitowy.
  - Proces rozruchu: Ładuje kod z MBR, brak wsparcia dla nowoczesnych standardów.

- **UEFI** (Unified Extensible Firmware Interface):
  - Wprowadza GPT (GUID Partition Table) – obsługa dysków >2 TiB, do 128 partycji.
  - Uruchamia bootloadery w trybie 64/32-bitowym (pominięcie trybu 16-bitowego).
  - Wymaga partycji **ESP** (EFI System Partition) z systemem plików FAT.
  - Zapewnia usługi: *boot services*, *runtime services*, *variable services* (przechowywanie kluczy i konfiguracji).

---

## 2. **GUID Partition Table (GPT)**
- **Struktura**:
  - **Protective MBR**: Zabezpiecza przed nadpisaniem przez starsze narzędzia.
  - **Nagłówek GPT**: Zawiera sumę kontrolną i rozmiar tablicy partycji.
  - **128 wpisów partycji**: Każdy zawiera GUID typu, nazwę (UTF-16), LBA początku i końca.

- **Przykładowe typy partycji**:
  - `C12A7328-F81F-11D2-BA4B-00A0C93EC93B` – partycja EFI (ESP).
  - `0FC63DAF-8483-4772-8E79-3D69D8477DE4` – partycja danych Linux.

---

## 3. **Secure Boot**
- **Cel**: Zabezpieczenie przed złośliwym oprogramowaniem podczas rozruchu.
- **Klucze**:
  - **PK (Platform Key)**: Główny klucz certyfikujący.
  - **KEK (Key Exchange Key)**: Używany do podpisywania baz `db` i `dbx`.
  - **db (Whitelist)**: Lista dozwolonych sygnatur/haszy.
  - **dbx (Blacklist)**: Lista zablokowanych sygnatur/haszy.

- **Tryby działania**:
  - **Setup Mode**: Brak PK – możliwość modyfikacji kluczy.
  - **User Mode**: PK aktywny – modyfikacje wymagają autoryzacji.
  - **Deployed Mode**: Brak możliwości modyfikacji.

- **Wsparcie w dystrybucjach Linuksa**:
  - **Debian/Ubuntu**: Wykorzystują *shim* (podpisany przez Microsoft) do ładowania GRUB-a.
  - **Fedora/RHEL**: Pełne podpisywanie jądra i modułów.

---

## 4. **Problemy z implementacją UEFI**
- **Przykłady**:
  - **Samsung (2013)**: Błędy w UEFI powodujące "brickowanie" laptopów przy bootowaniu Linuksa.
  - **Lenovo**: Sprawdzanie nazwy boot entry (np. "Windows Boot Manager") przed uruchomieniem.
  - **Boothole (2020)**: Luka w GRUB-ie umożliwiająca przejęcie kontroli przez złośliwy `grub.cfg`.

---

## 5. **Bootloadery w ekosystemie Linuksa**
- **GRUB 2**:
  - Obsługuje BIOS i UEFI.
  - Modularna architektura, konfiguracja przez `grub.cfg`.
  - W Debianie: Instalacja z pakietów `grub-efi-amd64` (UEFI) lub `grub-pc` (BIOS).

- **EFI Stub**:
  - Jądro Linuksa może być bezpośrednio uruchamiane przez UEFI (bez bootloadera).
  - Wymaga skopiowania `vmlinuz` i `initrd.img` do partycji ESP.

- **rEFInd**:
  - Graficzny boot manager dla UEFI z możliwością tematycznego dostosowania.
  - Wspiera automatyczne wykrywanie systemów operacyjnych.

- **SYSLINUX**:
  - Zbiór narzędzi do bootowania z różnych nośników (FAT, ext, CD, PXE).
  - Proste menu konfigurowalne przez plik `syslinux.cfg`.

---

## 6. **Konfiguracja GRUB-a w Debianie**
- **Struktura katalogów**:
  - `/boot/grub/`: Moduły, tematy, konfiguracja.
  - `/boot/efi/`: Partycja ESP z plikami `.efi`.

- **Narzędzia**:
  - `grub-install`: Instalacja bootloadera na dysk.
  - `update-grub`: Generowanie `grub.cfg` na podstawie szablonów.

- **Zaawansowane opcje**:
  - **Tematy**: Zmiana wyglądu menu przez pliki graficzne w `/boot/grub/themes/`.
  - **Własne skrypty**: Dodawanie wpisów do `grub.cfg` przez pliki w `/etc/grub.d/`.

---

## 7. **Podsumowanie**
- **UEFI** eliminuje ograniczenia BIOS-u, ale wprowadza nowe wyzwania (Secure Boot, zależność od producentów).
- **GPT** jest niezbędny dla dysków >2 TiB i nowoczesnych systemów.
- **Secure Boot** wymaga dostosowania dystrybucji Linuksa, ale można go obejść przez własne klucze.
- **Bootloadery** oferują różne podejścia: od prostych (EFI Stub) po zaawansowane (GRUB 2, rEFInd).



# Wykład 12 - Wirtualizacja
Na samym początku, w dos'ie,  większość rzeczy nie była wirtualizowana (może poza wyjątkiem pamięci, na której był system plików)  
teraz bardzo często system wirtualizuje dla programu procesor, pamięć, etc  

jak udawać?  
- symulacja - bardzo szczegółowe odtwarzanie działania systemu (jak procesor coś źle robi, to jest to oddawane)
- emulacja - ważny jest tylko efekt działania
- wirtualizacja - zwielokrotnianie platformy (udawanie wyłączności)  

## Symulacja
symulować możemy procesor, hardware albo system operacyjny  
przykład:
- verilog (program do udawania obwodów elektronicznych)  
- GNS3 (program do symulacji sieci komputerowych)

## Emulacja
procesory:
- QEMU (Quick Emulator) - szybki 
- MIPS - (fajne, bo można skonfigurować czy chcemy korzysać z big endian czy little endian)
- mikrokontrolery - np Arduino, ale słabo idzie implementacja interfejsów

systemy operacyjne:
- Wine
- WSL (ale nie WSL v2, który jest wirtualizowany)

hardware:
- FPGA (Field Programmable Gate Array) - programowalne układy scalone, które można skonfigurować do różnych zadań


## Emulacja vs wirtualizacja
> basic block - kawałek kodu maszynowego który ma jedno wejście i jedno wyjście (nie ma rozgałęzień)

- **qemu** jako emulator - przekłada na bieżąco basic blocki jednego procesora na drugiego (jest to wydajne, ale nie aż tak jak z kvm)  
- wirtualizacja **KVM**  
nie musimy przekładać np x86 na x86, tworzymy usługę jądra pracującego w trybie użytkownika (te jądro nawet o tym nie wie), tkzw **hypervisor**  
co jeżeli jądro chce skorzystać z rozkazów uprzywilejowanych?   
później nawet pojawiło się wsparcie sprzętowe dla hypervisorów,  czyli dodatkowe rozkazy  


## Jak zwirtualizować procesor?
potrzebujemy hypervisora - oprogramowanie które pozwala tworzyć i uruchamiać maszyny wirtualne (OS nazywano kiedyś hypervisorem lub monitorem)   
rodzaje:
- bare metal - działa bezpośrednio na fizycznej maszynie, bardzo podstawowy system operacyjny zwykle o architekturze mikrojądra  
  - zazwyczaj działa tak że się uruchamia (gospodarz) i od razu uruchamia maszynę wirtualną (gościa)
  - przykład: Xen (jazda bez trzymanki, potrafi dużo, po uruchomieniu odpala linuksa, który może nim zarządzać), VMware ESXi (płatne ale wygodne), Microsoft Hyper-V
- hosted - mamy system operacyjny pracujący na maszynie, a na nim uruchamiamy program do wirtualizacji
  - np: VirtualBox, Qemu, VMware Workstation
- hybrydowy - kompletny system operacyjny, który ma funkcjonalność hypervisora
  - np: KVM, FreeBSD bhyve, UML

## Pierwszy hypervisor
koncept jest taki że programy wykonywuja się w trybie nieuprzywilejowanym na procesorze, i w momencie próby skorzystania z rozkazów uprzywilejowanych, procesor zatrzymuje wykonwywanie i przekazuje kontrolę do hypervisora  
wtedy hyervisor może zmienić stan wirtualnego procesora i przekazać kontrolę z powrotem do procesora  

## Parawirtualizacja
do tej pory system gościa nie wiedział że jest gościem, ale można podejść do tego inaczej - stworzyć system operacyjny, który wie że jest gościem i nie korzysta z rozkazów uprzywilejowanych (zamiast nich może wywoływać SYSCALLE hypervisora)  
to się pojawiło przed wsparciem sprzętowym, po jego wprowadzeniu okazało się że prawdziwa wirtualizacja jest szybsza (ale np Xen dalej potrafi pracować w tym trybie, dodatkowo wiele systemów nadal ją wspiera)  

___

do tej pory wirtualizowaliśmy OS oraz emulowaliśmy peryferia, cierpi na tym np prędkość połączenia sieciowego  
wprowadzono więc wirtualizację w kartach sieciowych - w karcie tworzone są profile, tak że hypervisor nie musi uczestniczyć w komunikacji  

## Najpopularniejsze hypervisory
- stacje robocze
  - quemu, kvm, aquemu
  - sun, virtualbox
  - vmware workstation player, vmware workstation pro
- serwery
  - xen (najprostrze, najfajniejsze)
  - kvm (używają AWS, GCP, Azure)
  - vmware server
  - microsoft hyper-v
- zarządzanie
  - libvirt
  - vagrant


## chroot(2) i chroot(8)
polecenie chroot(8) zmienia katalog roboczy procesu, ale syscall chroot(2) nie  
po zrobieniu chroot idea jest taka, że nie możemy iść do góry  
powstał atak _change root escape_ - po ponownym wywołaniu chroot(2) na poziom niżej a potem chdir(2) ale na `..` udaje się przejść wyżej (bo root został przesunięty, więc oryginalne działanie `..` w tym miejscu przywrócone)

Zastosowanie off-label chroota - honeypots
- idea: książka clifforda stolla "the cuckoo's egg"
- wirtualne pułapki na atakujących, które mają ich zmylić
- tworzy honeypot używając chroot


## Kontenery - bezpieczniejszy chroot
w linuxie **kontener** oparty jest na namespace i cgroups (LXC, LXD, Docker, Snap, Flatpak, AppImage)  
mają one rozdzielone userlandy, ale współdzielą kernel  

## Namespace i cgroups
- **namespace** - przestrzeń nazw, która pozwala na izolację zasobów systemowych (PID, mount, network, IPC, user, UTS)
- rodzaje
  - `mnt`
  - `pid`
  - `net`
  - `ipc`
  - `uts`
  - `user`
  - `cgroup`

- **cgroups** - grupy kontrolne, które pozwalają na ograniczenie zasobów (CPU, pamięć, I/O) dla procesów w kontenerze
- rodzaje
  - `cpu` - ograniczenie zużycia czasu procesora
  - `memory` - ograniczenie zużycia pamięci
  - `blkio` - ograniczenie I/O
  - `devices`
  - ...


## Sandboxing vs kontenery
sandboxing - uruchamianie aplikacji w izolowanym środowisku, które ogranicza dostęp do systemu operacyjnego i zasobów (np. SELinux, AppArmor)  
kontenery nie zaperwniają bezpieczenistwa, dają inne środowisko (jailbreak - sposób na wyjście z kontenera)  

### Zwykłe vs nieuprzywilejowane konenery (LXC)



