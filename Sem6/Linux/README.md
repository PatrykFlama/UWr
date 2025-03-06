# Some notes

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


