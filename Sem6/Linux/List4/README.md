[(back)](../)

| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
|---|---|---|---|---|---|---|---|---|
| 1 |   | 1 | 1 | 1 | 1 |   |   |   |

# Lista 4

## Zadanie 1
Kolejność rozwijania poleceń w bashu:
1. rozwinięcia nawiasów wąsatych, np. `file{1,2,3}, file{1..10}`,
2. rozwinięcia tyldy, np. `~/Downloads/`,
3. rozwinięcia zmiennych, np. `$HOME`,
4. podstawienia instrukcji, np. `$(cat file.txt)`,
5. podstawienia procesów, np. `<(pdftops file.pdf -)`,
6. rozwinięcia arytmetyczne, np. `$((N+1))`,
7. (powtórny) podział na słowa,
8. rozwinięcia nazw plików (_globów_), np. `file?-*.txt`.

### czemu powtórny podział na słowa jest potzrebny
rozwinięte zmienne / podstawione instrukcje często zawierają ciągi znaków, które powinny być traktowane jako osobne argumenty  

#### czemu powinien być wykonywany po rozwinięciach zmiennych
```bash
FILES="file1.txt file2.txt"
ls $FILES
```
wpp ls próbowałby szukać pliku `file1.txt file2.txt`

#### czemu powienien być wykonywany po podstawieniach instrukcji
```bash
ls $(echo file1.txt file2.txt)
```
analogicznie jak wyżej

### czemu rozwinięcia instrukcji są wykonywane po rozwinięciach zmiennych

```bash
CMD="echo hello world"
$CMD
```
najpierw CMD jest rozwijany do `echo hello world`, następnie jest wykonywany - inaczej $CMD zostałby potraktowany jako jedno słowo


## Zadanie 3
w takim przypadku wykonanie lokalnego skryptu ma większy priorytet niż globalnego, dlatego jeżeli np do wyboru będzie skrypt lokalny `ls` to zostanie on wykonany zamiast globalnego `ls`  
wystarczy więc nasz nikczemny skrypt o takiej nazwie wrzucić do archiwum  

```bash
mkdir lolcats
echo -e '#!/bin/bash\necho "You’ve been pwnd!"\nsl' > lolcats/ls
chmod +x lolcats/ls
tar cf lolcats.tar -C lolcats .
```

## Zadanie 4
### podmiana pliku z PATH
jeżeli skrypt wywołuje polecenie, ale nie korzysta z jego pełnej ścieżki, to jest ona wyszukiwana w `$PATH`  
możemy więc podmienić plik w jednym z katalogów z `$PATH` na nasz złośliwy skrypt  

```bash
echo -e '#!/bin/bash\necho "You’ve been pwnd!"\nsl' > /usr/local/bin/ls
chmod +x /usr/local/bin/ls
```

### pliki SUID
pliki SUID są wykonywane z uprawnieniami właściciela, a nie użytkownika je uruchamiającego (przykład zastosowania: `/bin/passwd` który korzysta z `/etc/shadow` do którego dostęp ma tylko root - suid aby móc zmienić hasło bez uprawnień roota)    

np jeżeli root ustawił SUID na bash `chmod u+s /bin/bash` to każdy użytkownik, który uruchomi bash `/bin/bash -p` (-p aby zachować uprawnienia użytkownika), będzie miał uprawnienia roota

### dostęp do .bashrc
mając dostęp do .bashrc innego użytkownika, możemy w nim umieścić złośliwy skrypt, który zostanie wykonany przy każdym uruchomieniu basha przez tego użytkownika  


## Zadanie 5
### jak korzystać z `getopt`
```bash
ARGS=$(getopt -o cgw:hv -l capitalize,world,greeting:,color:,help,version -- "$@") || exit 1
```

* `-o` to krótkie opcje: `c`, `g`, ...; `:` oznacza że opcja przyjmuje argument
* `-l` to długie opcje: `capitalize` to długa forma `-c` 
* `--` to separator opcji od argumentów (wymusza interpretację następnych argumentów jako argumentów, nawet jeżeli zaczynają się od `-`)
* `"$@"` to lista argumentów przekazanych do skryptu - przekazujemy je do `getopt`
* `|| exit 1` to zakończenie skryptu jeżeli `getopt` zwróci błąd

```bash 
eval set -- "$ARGS"
```

`set -- "$ARGS"` ustawia argumenty skryptu na te zwrócone przez `getopt`


### jak kolorować w terminalu
możemy korzystać z ANSI escape codes, np. `\033[1;31m` to czerwony kolor, `\033[0m` to reset kolorów;  
* `\e` to znak escape który inicjalizuje sekwencję ANSI  
* `[1;31m` to kod formatujący - `1` to pogrubienie, `31` to czerwony kolor  
* `[0m` to reset kolorów
