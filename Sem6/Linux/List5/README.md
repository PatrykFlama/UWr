[(back)](../)

# List 5

| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 |
|---|---|---|---|---|---|---|---|---|----|----|
| 1 |   |   | 1 | 1 |   |   |   |   |    |    |


## Zad 1
```bash
#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <slee ptime>"
    exit 1
fi

sleep_time=$1

while read -r line; do
    echo "$line"
    sleep "$sleep_time"
done
```

### dwa niezależne procesy
```bash
./reader 1.0 < multiline.txt &
./reader 1.6 < multiline.txt &
```

### wspłdzielony deskryptor
```bash
exec 3<multiline.txt
./reader 1.0 <&3 &
./reader 1.6 <&3 &
```

### zapis niezależny
```bash
./reader 1.0 <multiline.txt >> output.txt &
./reader 1.6 <multiline.txt >> output.txt &
```

### zapis współdzielony deskryptor
ta metoda nie uszkodzi danych
```bash
exec 4>>output.txt
./reader 1.0 <multiline.txt >&4 &
./reader 1.6 <multiline.txt >&4 &
```


## Zad 2
aby dowiedzieć się jakichś informacji o pliku możemy skorzystać ze `stat(2)` (zamiast z `ls -l`)  
`truncate` służy do zmiany rozmiaru pliku, może być używane do skracania lub rozszerzania pliku  

zamiasta czegoś w stylu /dev/sda tak na prawdę urządzenia mają swoje majory i minory identyfikujące je

`ls`:  
* flaga `-S` – sortuje pliki według rozmiaru
* flaga `-t` – sortuje pliki według czasu modyfikacji
* flaga `-r` – odwraca kolejność sortowania


opcja montowania `no atime` oznacza że wartość czasu dostępu do plików nie będzie *zbyt często* modyfikowany  


linki symboliczne; zazwyczaj natykając się na lik symboliczny mamy 2 opcje: pójść po nim albo się zatrzymać i powiedzieć że to link symboliczyn  
więc mamy polecenia `relpath` oraz `readlink`   
`realpath` (który rozwija symlinki) jest w stanie podać ścieżkę do pliku który nie istnieje (co jest przydatne, bo może np dopiero jeszcze utworzymy ten plik)


`dirname` - zwraca katalog nadrzędny pliku  
`basename` - zwraca nazwę pliku bez katalogu nadrzędnego; możemy podać mu argument, który będzie usuwanym suffixem z nazwy, o ile istnieje/pasuje, np jego rozszerzenie  



## Zad 4
### `lsof`, `fuser`
narzędzia służą do przeglądania otwartych plików i związanych z nimi procesów

- **`lsof` (List Open Files)**  
  - pokazuje wszystkie otwarte pliki i powiązane z nimi procesy  
  - może wyświetlić otwarte pliki dla konkretnego procesu, użytkownika, portu sieciowego itp  
  - zapewnia bardziej szczegółowe informacje, np deskryptory plików i typy dostępu  
  - przykłady użycia:
    - `lsof -p <PID>` – pokazuje pliki otwarte przez dany proces  
    - `lsof <path>` – pokazuje, który proces otworzył dany plik  
    - `lsof -u <user>` – pokazuje pliki otwarte przez danego użytkownika

```bash
# wszystkie otawrte pliki na systemie
lsof

# przez proces
lsof -p PID

# jakie procesy otworzyły dany plik/folder
lsof path
lsof +D path

# pliki otwarte przez użytkownika
lsof -u user

# wszystkie połączenia sieciowe na porcie
lsof -i :port
```

- **`fuser` (File User)**  
  - koncentruje się na identyfikacji procesów używających danego pliku lub zasobu  
  - może być używane do wymuszania zamknięcia procesów korzystających z pliku  
  - przykłady użycia:
    - `fuser <path>` – wyświetla PID-y procesów używających danego pliku  
    - `fuser -u <path>` – dodatkowo pokazuje użytkownika danego procesu  
    - `fuser -k <path>` – zabija procesy korzystające z danego pliku  

```bash
# procesy korzystające z pliku
fuser path

# procesy korzystające z pliku i ich użytkownicy
fuser -u path

# zabij wszystkie procesy korzystające z pliku
fuser -k path
```

### różnice
- `lsof` pokazuje więcej informacji o plikach i procesach
- `fuser` koncentruje się na identyfikacji procesów korzystających z pliku, może być używane do wymuszania zamknięcia tych procesów

### Jak ujawnić wszystkie pliki otwarte przez podany proces?  
`lsof -p <PID>`

### Jak sprawdzić, który proces otworzył dany plik?  
`lsof <ścieżka>` lub `fuser <ścieżka>`

### Jak ujawnić wszystkie pliki otwarte przez podanego użytkownika?  
`lsof -u <użytkownik>`


### użytyczne zastosowania
`lsof` - gdy chcemy odmontować system plików, ale nie możemy tego zrobić, bo coś go używa  


## Zad 5
`strace` służy do śledzenia wywołań systemowych procesów, umożliwia analizowanie interakcji programów z jądrem systemu (co jest przydatne w debugowaniu, optymalizacji i diagnozowaniu problemów)

## Podstawowe scenariusze użycia
1. śledzenie uruchamianego programu
```sh
strace ./program
```

wyświetla wszystkie wywołania systemowe wykonane przez program

2. śledzenie istniejącego procesu
```sh
strace -p PID
```

pozwala monitorować działający proces o podanym PID

3. zapis wyjścia do pliku
```sh
strace -o output.txt ./program
```

4. filtrowanie wywołań systemowych
```sh
strace -e open,read,write ./program
```

pokazuje tylko wywołania sysstemowe open, read i write

5. pomiar czasu wykonania systemowych wywołań
```sh
strace -T ./program
```

6. sytatystyki wywołań systemowych
```sh
strace -c ./program
```

## Przykład
```bash
#!/bin/bash

while true; do
    echo "Logging..." >> strace_demo.log
    cat /proc/uptime > /dev/null
    sleep 1
done
```


## Zad 6
zadaine: "zrób sobie pipe'a ale takiego nazwanego, i do niego sobie pisz"   
- `mkfifio(1)` - tworzy plik ktory jest pipem  
- `mkfifo sada` - tworzy plik o nazwie sada, który jest pipe'm  
- `debugfs` - narzędzie do debugowania systemu plików ext2/ext3/ext4  
- `cat < sada` - odczytuje dane z pipe'a  
- `echo "Hello" > sada` (w innym terminalu) - zapisuje dane do pipe'a, komenda `cat < sada` odczyta te dane i się skończy (bo dostanie EOF)  
- 
