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

(wykresy są w folderze `/var/lib/mystat`)
