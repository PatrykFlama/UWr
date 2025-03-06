# Lista 2
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 |
|---|---|---|---|---|---|---|---|---|----|----|----|----|
| X | X | X | X | X | X |   |   |   | T  | T  |    |    |

## Zad 1
### `cloc` – Count Lines of Code  
**cloc - Count, or compute differences of, lines of source code and comments.**   
* liczy liczbę plików i linii kodu w projekcie
* rozróżnia języki programowania i osobno traktuje komentarze oraz puste linie
* obsługuje **różne formaty** plików, w tym `.c`, `.cpp`, `.py`, `.java`, `.sh`, itd.  


### `sloccount` - analiza kodu i szacowanie kosztów
**sloccount - count source lines of code (SLOC)**  
* wykrywa język programowania dla każdego pliku.
* grupuje wyniki według katalogów.
* oblicza czas pracy (osobolata) i koszt na podstawie modelu COCOMO.

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
alias gentmp='date "+tmp-%Y%m%d%H%M%S"'
```

date [OPTION]... [+FORMAT] - wyświetla datę w podanym formacie  
format poprzedzamy znakiem `+`  

### `genpwd`
`/dev/urandom` - losowa wartość, w przeciwieństwie do `/dev/random` nie blokuje się, gdy zabraknie entropii  

```bash
alias genpwd='< /dev/urandom tr -dc "3-9A-HJ-NP-Z" | head -c 32; echo'
```

bierzemy strumień losowych znaków, usuwamy te które nam nie pasują, pozostałość skracamy do 32 znaków

## Zad 3 - `grep`
### Wyrażenia regularne
**print lines that match patterns**  
`grep` wykorzystuje wyrażenia regularne (regex) do wyszukiwania wzorców w tekście

* **Znaki specjalne:**
  * `.` – dowolny znak (np `a.c` znajdzie `abc`, `adc`, `a-c`)
  * `^` – początek wiersza (np `^abc` znajdzie tylko linie zaczynające się od "abc")
  * `$` – koniec wiersza
  * `*` – powtórzenie 0 lub więcej razy (np `ab*c` pasuje do `ac`, `abc`, `abbc`)
  * `+` – powtórzenie 1 lub więcej razy (np `ab*c` pasuje do `abc`, `abbc`) **ERE**
  * `?` – opcjonalny znak (np `ab?c` pasuje do `ac`, `abc`) **ERE**
  * `\` – znak escape (np `\.` pasuje do `.`)
  * `[abc]` – odpowiada jednej literze z podanego zbioru
  * `[a-z]` - oznacza dowolną literę od a do z (kolejność liter wg ASCII)

### **Podstawowe opcje `grep`**
`grep [opcje] wzorzec [plik]` – przeszukuje plik w poszukiwaniu wzorca
* `-E`  – włącza rozszerzone wyrażenia regularne **ERE** (odpowiada poleceniu `egrep`)
* `-i` – ignoruje wielkość liter (np `grep -i "abc"` odpowiada `grep "[aA][bB][cC]"`)
* `-v` – odwraca wynik, zwraca linie **niepasujące** do wzorca
* `-c` – zwraca tylko liczbę pasujących linii
* `-n` – pokazuje numer linii z dopasowaniem
* `-r` lub `-R` – rekurencyjne przeszukiwanie katalogów
* `-l` – wypisuje tylko nazwy plików zawierających dopasowanie
* `-F` – traktuje wzorzec jako zwykły ciąg znaków a nie regex (`fgrep`)
* `-o` – wypisuje tylko dopasowane fragmenty linii (nie całe linie, każdy fragment w nowej linii)

## Zad4 - `find`
**search for files in a directory hierarchy**  
`find` służy do wyszukiwania plików i katalogów

### opcje
`find [opcje] [ścieżka]` – przeszukuje podaną ścieżkę (np. `.` dla bieżącego katalogu, `/` dla całego systemu)
 
<!-- * `-L` – śledzi linki symboliczne -->
* `-maxdepth n` – ogranicza głębokość przeszukiwania do `n`
* `-mindepth n` – minimalna głębokość przeszukiwania (wcześniej nie szuka)
* `-mount` lub `-xdev` – nie przechodzi do innych systemów plików
* `-delete` – usuwa znalezione pliki
* `-exec <komenda> {} ;` – wykonuje podaną komendę na każdym znalezionym pliku, `{}` odpowiada nazwie znalezionego pliku, `;` terminuje ciąg argumentów komendy
* `-prune` – nie przechodzi do podkatalogów

### Filtry
> `n`  - dokładnie `n`
> `+n` - więcej niż `n`  
> `-n` - mniej niż `n`

* `-amin n` lub `-amin plik` - filtr ostatniego dostępu do pliku (minuty lub względem dostępu do innego pliku) 
* `-atime n` – filtr ostatniego dostępu do pliku (dni)

* `-name "nazwa"` – wyszukuje pliki o podanej nazwie (uwzględnia wielkość liter)
* `-iname "nazwa"` – wyszukuje pliki o podanej nazwie (ignoruje wielkość liter)
* `-path ścieżka` – wyszukuje pliki o podanej ścieżce 

* `-type d/f/l` – wyszukuje tylko katalogi/pliki/linki symboliczne
* `-size [+-]n[cwbkMG]` – wyszukuje pliki o podanym rozmiarze
* `-user użytkownik` – wyszukuje pliki należące do określonego użytkownika


## Zad 5 - `rename`
**renames multiple files**  

`rename [opcje] '[sy]/wzorzec/zamiana/' pliki` – zmienia nazwy plików zgodnie z podanym wzorcem

* `s` - substytucja (działa jak sed)
* `y` - translacja (działa jak tr)

### opcje
* `g` - globalne zastosowanie zmiany dla każdego wystąpnienia w nazwie (domyślnie zmienia tylko pierwsze wystąpienie) np `s/a/b/g` zmieni `aaa.txt` na `bbb.txt`  

### flagi
* `-d` - tylko zmień nazwę plików, nie folderów
* `-e` - wywołuje polecenie na każdym pliku  


## Zad 6
* zainstalowane bez katalogu  
`dpkg --get-selections` wypisze zainastalowane pakiety,  
`awk '{print $1}'` wybierze tylko pierwszą kolumnę z linii (czyli nazwę pakietu),  
sortujemy aby porównać (wymagane)  
porównanie za pomocą `comm -23`, który przyjmuje 2 porównywane strumienie   
`-2` ukrywa wiersze które występują tylko w drugiej liście,  
* `-3` ukrywa wiersze które występują w obu listach 
więz zostawia te pakiety które są tylko w pierwszej liście  

```bash
comm -23 <(dpkg --get-selections | awk '{print $1}' | sort) <(ls /usr/share/doc | sort)
```

* katalogi bez pakietu oraz nazwy pakietu-właściciela
początek jest podobny
bierzemy nazwy pakietów, ale tym razem korzystamy z opcji `comm -13` czyli mamy wiersze tylko z 2 listy  
dodajemy polecenie `xargs` które wykonuje polecenie dla każdego argumentu,  
`-I{}` oznacza że `{}` to argument, 
`dpkg -S /usr/share/doc/{}` - znajduje pakiet do którego należy plik w `/usr/share/doc/nazwapakietu`

```bash
comm -13 <(dpkg --get-selections | awk '{print $1}' | sort) <(ls /usr/share/doc | sort) | xargs -I{} dpkg -S /usr/share/doc/{}
```

* lista pakietów posiadających kataloga ale nie `changelog.Debian.gz`
korzystamy z poprzedniego polecenia aby uzyskać listę pakietów,  
następnie wypisujemy te które nie posiadają pliku `changelog.Debian.gz`

```bash
for pkg in $(comm -23 <(dpkg --get-selections | awk '{print $1}' | sort) <(ls /usr/share/doc | sort)); 
do
  if [ ! -f "/usr/share/doc/$pkg/changelog.Debian.gz" ]; then
    echo "$pkg"
  fi
done
```

* ... posiadające `changelog.Debian.gz` i mające tylko 1 wpis
korzystany z `zcat` bo musimy zdekompresowac pluik

```bash
for pkg in $(ls /usr/share/doc);
do
  if [ -f "/usr/share/doc/$pkg/changelog.Debian.gz" ]; then
    count=$(zcat "/usr/share/doc/$pkg/changelog.Debian.gz" | grep -c '^ ')
    echo $count
    # [ "$count" -eq 1 ] && echo "$pkg"
  fi
done
```

* liczba wystąpień słowa `bash` w pliku `/usr/share/doc/bash/INTRO.gz`
```bash
zcat /usr/share/doc/bash/INTRO.gz | grep -io 'bash' | wc -l
```