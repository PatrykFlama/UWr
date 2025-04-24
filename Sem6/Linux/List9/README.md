# Lista 9

## Zad 1
Plik [BSD init](./FreeBSD/init) `/usr/local/etc/rc.d/mystat`

Plik [konfiguracyjny](./FreeBSD/config) `/usr/local/etc/default/mystat`

instalacja skryptu:
```bash
chmod 755 /usr/local/etc/rc.d/mystat
```

tworzenie pliku konfiguracyjnego
```bash
mkdir -p /usr/local/etc/default
echo 'mystat_enable="YES"' > /usr/local/etc/default/mystat
```

włączanie serwisu:
```bash
sysrc mystat_enable="YES"
```

zarządzanie
```bash
service mystat start
service mystat rotate  # wysyła SIGHUP
service mystat status
```



## Zad 4
Plik [mystat-graph.sh](./SystemD/mystat-graph.sh) `/usr/local/bin/mystat-graph.sh`

Oraz pozostałe pliki w `/etc/systemd/system`


```bash
# skrypt
sudo chmod +x /usr/local/bin/mystat-graph.sh

# katalog wynikowy
sudo mkdir -p /var/lib/mystat

# przeładuj systemd aby wczytać konfigurację
sudo systemctl daemon-reload

# włącz i uruchmo mystat
sudo systemctl enable mystat.service
sudo systemctl start mystat.service

# rysowanie na timer
sudo systemctl enable mystat-graph.timer
sudo systemctl start mystat-graph.timer
```

```bash
# sprawdź status
systemctl status mystat.service

# ostatnie 50 linii logów
journalctl -u mystat.service -n 50

# wymuś wygenerwanie wykresu
sudo systemctl start mystat-graph.service

# sprawdź timer
systemctl list-timers mystat-graph.timer
```

(wykresy są w folderze `/var/lib/mystat`, w gnome można otworzyć za pomocą `eog`)

_____

wszystkie serwisy oraz ich zależności możemy wyświetlić za pomocą
```bash
systemctl list-dependencies
```
stąd też wybierzemy serwis po którym chcemy uruchomić nasz serwis (np journalctl)  




# some notes
> można sobie do zad 2 odpaliś starego debiana

> twi normalnie instaluje debiana za pomocą dbootstrap

## Zad 1 - bsd init
linux wziął 'standardy' składni z systemu 5  
`ps -aux` - wypisuje wątki
- w kwadratowych nawiasach będą wątki jądra  
- proces 0 - jądro
- proces [idle] zjada 100% procesora  
- proces 1 - init (zazwyczaj), wg konwencji root/kernel jest jego rodzicem 

csh - c shell (składania podobna do c)    
tcsh - tenex c shell (rozwinięcie csh zrobione przez firmę tenex)  

`rcorder` - sortuje pliki

w FreeBSD domyślnym managerem pakietów jest `pkg`  
ma on są 2 rodzaje pakietów:
- current - update na bieżąco
- ported - update raz na 3 msc

defaultowa konfiguracja daemona (tworzona w trakcie jego instalacji) powinna być w `/etc/defaults/`  
jeżeli administrator chce coś zmienić w konfiguracji, dopisuje to w `/etc/rc.conf.d/`  
np defaultowo po zainstalowania daemon ssh powinien być wyłączony i powinien byc włączony dopiero przez użytkownika/admina (w osobnym miejscu niż instalacji)  


w freebsd musimy sami się zdaemonizować (odpiąć od terminala, zabezpieczenie żeby daemon nie był odpalony 2 razy, etc) - `void daemonize()`  


tak na prawdę napisanie konfiguracji daemona składa się z:
- w rc.d mamy skrypty start/stop etc daemona
- w /usr/local/etc/default/rc.conf mamy jego defaultową konfigurację

domyślnie w FreeBSD nie ma zamontowanego `/proc` (uważane jest za niebezpieczne), ale można je domontować żeby mieć kompatybilność z linuxem  

> zfs działa zarówno pod freebsd jak i linuxem, więc jest to dobra opcja jeżeli ktoś chce mieć dysk który działa tu i tu (np pendrive)  
> ale w ogólności jest to overkill dla prostych urządzeń tj laptopy  

> używanie syntax=on dla vim na root jest uważane za niebezpieczne  
> plaintext injection (po dodaniu modelinów)  

`/etc/rc.subr` - 

### /user/local/etc/
rotate_cmt="mystat_rotate" - komenda do rotwania plików z logami  



## Zad 2 - SysV init
bardziej robudowana werjsa systemu init, w porównaniu do tego z freebsd, wywodzi się ona z systemu 5  

tutaj już `/sbin/init` robi dużo rzeczy sam  
np zagląda do `/etc/init.d/` - tutaj każdy daemon ma swój plik (skrypt) startowy. musi on implementować start/stop etc więc też jest polecenie `service` (`type service`)  
w `/etc/init.d/skeleton` jest szablon do stworzenia naszego skryptu  

run levele (dla default-start/stop) - 1 single user mode; 6 restart; S przed wszystkimi (więc tak na prawdę shutdown to przełączanie się do odpowiedniego run levela)  

mamy `start-stop-daemon`'a, który już np jest w stanie zadbać o pid file (aczkolwiek preferujemy 'poprawnie' zdaemonizowany program, który się sam daemonizuje)  

> pakiety linux-utils oraz bsd-utils (zawierają użyteczny set narzędzi)


### katalogi /etc/rcX.d X = 1..6
jest to kolejność, która jest wyliczana automatycznie przez `insserv`  

`update.rc.d` - służy do włączania i wyłączania serwisów (jest to niskopoziomowe polecenie)  

> w skryptach nie użwyamy `.` - piszemy `source`   
> tak samo jest z backtickami - korzystamy z `$()`  


wg konwencji w pliku konfiguracyjnym defaulty powinny być wpisane i zakomentowane (dla ławtości edycji dla użytkownika)



## Zad 3,5 - openrc
jak patrzymy na openrc to warto poparzeć na system 5  
podobnie jak w bsd  
ma usługę `supervise-daemon` która nadzoruje swoją usługę, jeżeli ta 'zdechnie' to ją 'reanimuje'  
jest to d-d na to ze możemy mieć 'zaawansowany' system init bez tak skomplikowanego systemu jak systemd  



