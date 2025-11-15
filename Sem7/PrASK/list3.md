# Lista 3

## Zad 1, 2
dodajemy w `/etc/network/interfaces` (lub w folderze `interfaces.d`) przypisanie do naszego interfejsu:
```conf
iface ens18 inet6 static
    address 2a0b:5485:1:18::2
    netmask 64
    gateway 2a0b:5485:1:18::1
```

sprawdzamy pingując google po ipv6 `ping -6 google.com`

## Zad 3
dodamy do nginx tylko nasłuchiwanie również na ipv6: `listen [::]:80` oraz `listen [::]:443 ssl`

dodajemy również rekordy ipv6 do bind9
```conf
        IN      AAAA    2a0b:5485:1:18::2
ns1     IN      AAAA    2a0b:5485:1:18::2
ns2     IN      AAAA    2a0b:5485:1:17::2

@       IN      AAAA    2a0b:5485:1:18::2
*       IN      AAAA    2a0b:5485:1:18::2
```



