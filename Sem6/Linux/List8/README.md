[(back)](../)

# List 8
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 |
|---|---|---|---|---|---|---|---|---|----|----|----|----|----|----|
|   | X | X | X | X |   |   |   |   |    |  X |    |    |  X |  X |



## Zad 1
xterm - emulator terminala  
tworzy pseudoterminale: mater (pts) i slave (`/dev/pts/0` zachowuje się jak urządzenie znakowe), które natomiast odpala basha (`/bin/bash`)  
master i slave siedzą w kernelu (są przez niego obsługiwane)  

> historia nazwy SIGHUP (signal hangup) (odłożenie słuchawki)   
```text
---------
| modem ||D0-15|--------|D8-9 (RS282)| /dev/tty0 --- /bin/bash
---------
```


eksperymenty:
1. ---
bash odpalił jak swój podproces okulara, po zamknięciu basha SIGHUP nie jest wysyłany do okulara  
okular był dzieckiem basha, ale tego basha już nie ma, więc zostaje on przepięty do systemd.user (jest on ripperem), który jest przydzielony do zalogowanego użytkownika  
czasem zdarza się że proces przejdzie do zwykłego systemd (zależy od konfiguracji)  
> systemd jest odpowiedzialne za zarządzanie procesami  

po zabiciu xterma, kernel wyśle okularowi SIGHUP

więcej o sygnałach możemy znaleźć w *man bash(1)* w dziale *SIGNALS*  

`shopt` - shell options (konfiguracja ustawień/zachowań shella)  

`&!` - od razu wydziedzicza uruchomiony program


## Zad 2 - screen(1)
screen - manager 'okien' terminala  

- `screen -S <nazwa>` - tworzy sesję o nazwie `<nazwa>`
- `screen -ls` - wyświetla listę sesji
- zamknięcie karty `ctrl + a` `k` (kill)

### odłączanie/dołączanie do sesji
- odłączanie od bieżącej sesji: `ctrl + a` `d`
- dołączanie do sesji: 
  - `screen -r` - dołącz do ostatniej sesji
  - `screen -r <id>` - dołącz do sesji `<id>` (sprawdzane za pomocą `screen -ls`)

### przełączanie między kartami
- nowa karta `ctrl + a` `c`
- lista okien `ctrl + a` `"`
- przełączanie między kartami `ctrl + a` `n` (next) / `p` (previous)
- pokazanie numeru aktualnej karty `ctrl + a` `N`
- przełącznie do karty o numerze `n` `ctrl + a` `n` (gdzie `n` to cyfra od 0 do 9)

### zaznaczanie tekstu i kopiowanie do schowka
- wejdź w tryb kopiowania `ctrl a` `[`
- za pomocą strzałek zaznacz tekst
- `spacja` - zaznacz tekst
- `enter` - skopiuj zaznaczony tekst
- `ctrl + a` `]` - wklej skopiowany tekst

(schowek jest lokalny dla sesji screen)

### przykład
```bash
while true; do
    echo "Screen X Time: $(date '+%H:%M:%S')"
    sleep 1
done
```


## Zad 3 - tmux(1)
tmux - terminal multiplexer

- `tmux new -s <nazwa>` - tworzy sesję o nazwie `<nazwa>`
- `tmux ls` - wyświetla listę sesji
- zamknięcie karty `ctrl + b` `&` (kill) lub `tmux kill-session -t <nazwa>` 

### dołączanie/odłączanie od sesji
- `tmux new-session -s <nazwa>` - tworzy nową sesję o nazwie <nazwa>
- odłączanie od bieżącej sesji: `ctrl + b` `d`
- dołączanie do sesji: 
  - `tmux attach` - dołącz do ostatniej sesji
  - `tmux attach -t <id>` - dołącz do sesji `<id>`

### przełączanie między kartami
- nowa karta `ctrl + b` `c`
- lista kart `ctrl + b` `w`
- przełączanie:
  - `ctrl + b` `n` (next) / `p` (previous)
  - `ctrl + b` [`0` - `9`] - przełącz do karty o numerze `n`
  - `ctrl + b` `l` - przełącz do ostatniej karty
- pokazanie numeru aktualnej karty `ctrl + b` `q`

### zaznaczanie i kopiopwanie tekstu
- wejdź w tryb kopiowania `ctrl + b` `[`
- za pomocą strzałek zaznacz tekst
- `ctrl+spacja` - rozpocznij zaznaczanie
- `alt+enter` - skopiuj zaznaczony tekst (showek lokalny dla tmux)
- `ctrl + b` `]` - wklej skopiowany tekst

### dodatkowe funkcje
- podział ekranu na panele
  - poziomo `ctrl + b` `"`
  - pionowo `ctrl + b` `%`
  - przełączanie między panelami `ctrl + b` + strzałki

tmux pozwala na połączenie się z wielu terminali do tego samego okna (sesji)  


## Zad 4

- `trap <sygnał> <komenda>` - ustawia pułapkę na sygnał `<sygnał>` i wykonuje `<komenda>`
- `trap -p` - pokazuje ustalone pułapki  
- `trap -l` - pokazuje listę sygnałów

```bash
#!/bin/bash

while true; do
    message="Logging current date: $(date)"
    logger -t "monitor" "$message"  # syslog
    echo "$message"                 # stdout
    sleep 2
done
```

użycie `nohup`:
```bash
nohup ./mylogger.sh &
```

stdout będzie w pliku `nohup.out` w katalogu, z którego uruchomiono skrypt  


```bash
#!/bin/bash

# ignore SIGHUP, trap SIGUSR1
trap '' HUP
trap 'logger -t "monitor" "SIGUSR1 - terminating"; exit 0' USR1


while true; do
    logger -t "monitor" "Logging current date: $(date)"
    sleep 2
done
```

> czytanie z logów:  
> `journalctl -t 'monitor'`  
> `grep 'monitor' /var/log/syslog`



## Zad 5
### progra 
```bash
#!/bin/bash

if [ $# -ne 1 ]; then
    echo "Usage: $0 <number_of_processes>"
    exit 1
fi

cpu_load() {
    while true; do
        # bc - An arbitrary precision calculator language
        echo "sqrt(99999999)" | bc > /dev/null
    done
}

for i in $(seq 1 "$1"); do
    cpu_load &
done

trap 'kill $(jobs -p); exit' SIGINT
echo "Ctrl+C to stop..."
wait
```

### nice
```bash
nice -n 19 ./cpu_burner 12 # niski priorytet
sudo nice -n -19 ./cpu_burner 12 # wysoki priorytet (ujemny wymaga roota)

pgrep -f "cpu_burner"
renice -19 -p <PID> # zmiana priorytetu
ps -o pid,ni,comm -p <PID> # sprawdzenie priorytetu
kill -SIGSTOP <PID> # wstrzymanie procesu
kill -SIGCONT <PID> # wznowienie procesu
```

## Zad 6

## Zad 11 - `.bashrc.d`
tworzymy katalog `~/.bashrc.d/`  

w tym katalogu tworzymy pliki `XX_nazwa.sh` (`XX` - kolejność ładowania)

do `~/.bashrc` dodajemy:
```bash
if [ -d "~/.bashrc.d" ]; then
    for file in ~/.bashrc.d/*.sh; do
        if [ -f "$file" ]; then  # sprawdzamy czy istnieje
            source "$file"
        fi
    done
fi
```

przykładowe rozdzielenie `.bashrc` do podplików:
- 10_shell_opts.sh
- 20_prompt.sh
- 30_aliases.sh
- 40_completion.sh



## Zad 14 - top(1)
top - monitoruje procesy i zasoby systemowe

### podstawowe info
- q - wyjście
- h - pomoc
- k - zabicie procesu
- `Spacja` - odświeżenie

### sortowanie, wyświetlanie
- P - sortowanie po CPU
- M - sortowanie po pamięci RAM
- T - sortowanie po czasie działania
- x - podświetl kolumnę sortowania
- Z - zmiana kolorów interfejsu
- W - zapisanie konfiguracji (do `~/.toprc`)
- f - zarządzanie polami
  - d - włącz/wyłącz pole
  - s - wybierz do sortowania

### podsumowanie systemu
- l - średnie obciążenie
- 1 - szczegółowe info o rdzeniach CPU
- t - widok cpu
- m - widok ram
- H - wątki/procesy

### filtrowanie
- u - filtrowanie po użytkowniku

## Zad 15
### `ps`
`ps` - wyświetla procesy bieżącej sesji terminala:  
- `-f` - pełny format
- `-e` `-A` - wszystkie procesy
- `-u <user>` - procesy danego użytkownika
- `ps -eo pid,user,%cpu,cmd --sort=-%cpu` - wszystkie procesy z CPU i sortowanie po CPU


### `pstree`
`pstree` - wyświetla drzewo (wszystkich) procesów  
- `-p` - pokazuje PID
- `-u` - pokazuje użytkownika
- `pstree <user>` - procesy danego użytkownika
- `-h` - podświetla aktualny proces i jego potomków
- `-H <proces>` - podświetla dany proces i jego potomków




# Side notes
zazwyczaj powłoki kończąc pracę wysyłają do swoich dzieci SIGHUP (co ciekawe bash tego nie robi)  

ogólnie zaleca się z korzystanie z xterma, a nie ze screena, bo screen za bardzo się już rozrósł  
np przy screenie istnieje opcja (którą co prawda trzeba włączyć), która pozwala na współdzielenie terminala (obie osoby widzą to samo) - jest to całkiem niebezpieczne   

`nohup` - execve nie zmienia masek sygnałów, więc wystarczy że nohup będzie maskować SIGHUP a potem wywoływać dany program za pomocą execve (+ przekierwanie wejście/wyjścia)

## tmux fajny config

