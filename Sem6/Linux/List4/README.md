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

## Zadanie 7
[How to write man page](https://stackoverflow.com/questions/33049205/how-to-make-a-man-page-for-my-shell-script)

```troff
.\"Modified from man(1) of FreeBSD, the NetBSD mdoc.template, and mdoc.samples.
.\"See Also:
.\"man mdoc.samples for a complete listing of options
.\"man mdoc for the short list of editing options
.\"/usr/share/misc/mdoc.template
.Dd 19/3/25               \" DATE
.Dt hwb(1)      \" Program name and manual section number
.Os Debian
.Sh NAME                 \" Section Header - required - don't modify
.Nm hwb
.\" The following lines are read in generating the apropos(man -k) database. Use only key
.\" words here as the database is built based on the words here and in the .ND line.
.\" Use .Nm macro to designate other names for the documented program.
.Nd "Hello World Bash" program for greeting users
.Sh SYNOPSIS             \" Section Header - required - don't modify
.Nm
.Op Fl whv              \" [-abcd]
.Op Fl c Ar color_mode         \" [-a path]
.Op Ar g Ar greet_msg              \" [file]
.Ar name1                 \" Underlined argument - use .Ar anywhere to underline
name2 ...                 \" Arguments
.Sh DESCRIPTION          \" Section Header - required - don't modify
Comand .Nm greets given users and allows for some nice custom formatting.

.Sh OPTIONS
.Sh EXIT STATUS
.Sh EXAMPLE
.Sh AUTHORS
.Sh SEE ALSO

.Pp                      \" Inserts a space
A list of items with descriptions:
.Bl -tag -width -indent  \" Begins a tagged list
.It item a               \" Each item preceded by .It macro
Description of item a
.It item b
Description of item b
.El                      \" Ends the list
.Pp
A list of flags and their descriptions:
.Bl -tag -width -indent  \" Differs from above in tag removed
.It Fl a                 \"-a flag as a list item
Description of -a flag
.It Fl b
Description of -b flag
.El                      \" Ends the list
.Pp
.\" .Sh ENVIRONMENT      \" May not be needed
.\" .Bl -tag -width "ENV_VAR_1" -indent \" ENV_VAR_1 is width of the string ENV_VAR_1
.\" .It Ev ENV_VAR_1
.\" Description of ENV_VAR_1
.\" .It Ev ENV_VAR_2
.\" Description of ENV_VAR_2
.\" .El
.Sh FILES                \" File used or created by the topic of the man page
.Bl -tag -width "/Users/joeuser/Library/really_long_file_name" -compact
.It Pa /usr/share/file_name
FILE_1 description
.It Pa /Users/joeuser/Library/really_long_file_name
FILE_2 description
.El                      \" Ends the list
.\" .Sh DIAGNOSTICS       \" May not be needed
.\" .Bl -diag
.\" .It Diagnostic Tag
.\" Diagnostic informtion here.
.\" .It Diagnostic Tag
.\" Diagnostic informtion here.
.\" .El
.Sh SEE ALSO
.\" List links in ascending order by section, alphabetically within a section.
.\" Please do not reference files that do not exist without filing a bug report
.Xr a 1 ,
.Xr b 1 ,
.Xr c 1 ,
.Xr a 2 ,
.Xr b 2 ,
.Xr a 3 ,
.Xr b 3
.\" .Sh BUGS              \" Document known, unremedied bugs
.\" .Sh HISTORY           \" Document history if command behaves in a unique manner
```