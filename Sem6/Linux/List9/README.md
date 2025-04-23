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


