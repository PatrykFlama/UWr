# Lista 2
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 |
|---|---|---|---|---|---|---|---|---|----|----|----|----|
|   | X |   |   |   |   |   |   |   |    |    |    |    |

## Zad 1
### `cloc`
**cloc - Count, or compute differences of, lines of source code and comments.**  

### `sloccount`
**sloccount - count source lines of code (SLOC)**

## Zad 2
### `ll`
```bash
alias ll='ls -lAFbhv --color=always | less -XFR'
```

* `-l` - wyświetla listę plików w formie listy
* `-A` - wyświetla wszystkie pliki, poza `.` i `..`
* `-F` - dodaje znaki specjalne do nazw plików (np `*` dla wykonywalnych, `/` dla katalogów)
* `-b` - wyświetla znaki specjalne w formie znaków ASCII
* `-h` - wyświetla rozmiar plików w formie czytelnej dla człowieka
* `-v` - sortuje pliki w sposób 'naturalny'
* `--color=always` - zawsze koloruje wynikowe pliki

* `-X` - nie czyści ekranu po zakończeniu `less`
* `-F` - automatyzcnie kończy `less` jeżeli wynik jest krótki
* `-R` - pozwala na wyświetlanie kolorów

### `gentmp`
```bash
alias gentmp=`date "+tmp-%Y%m%d%H%M%S"'
```

date [OPTION]... [+FORMAT] - wyświetla datę w podanym formacie  
format poprzedzamy znakiem `+`  

### `genpwd`
`/dev/urandom` - losowa wartość, w przeciwieństwie do `/dev/random` nie blokuje się, gdy zabraknie entropii  

```bash
< /dev/urandom tr -dc '3-9A-HJ-NP-Z' | head -c 32; echo
```

bierzemy strumień losowych znaków, usuwamy te które nam nie pasują, pozostałość skracamy do 32 znaków