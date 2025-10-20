# List 2
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
|---|---|---|---|---|---|---|---|---|----|
| x | x | x | x | x |   |   |   |   |    |


## Zadanie 1
> Pokaż, w jaki sposób skonfigurować proxy pełniące rolę cache dla pacman/apt. Czemu chcielibyśmy to
> robić? Pomyśl o sytuacji, gdy uruchamiasz CI bazujący na obrazie maszyny wirtualnej, w którym jednym
> z kroków jest aktualizacja paczek systemowych do najnowszej wersji.
> Zademonstruj, że Twoja konfiguracja działa (np. z użyciem maszyny wirtualnej). Niech Twój host robi
> za cache/proxy, a maszyna niech spróbuje zaktualizować pakiety. Pokaż, że pakiety brakujące w cache
> hosta są ściągane, zapisywane i udostępniane maszynie wirtualnej bez błędów.  

Jeżeli mamy wiele maszyn i każda z nich ma ściągać te same pakiety, to użycie cache/proxy zmniejsza ruch sieci zewnętrznej
oraz przyspiesza aktualizację pakietów, ponieważ pobierane są one z lokalnego cache.  

[Package proxy cache](https://wiki.archlinux.org/title/Package_proxy_cache)

[Apt-Cacher-NG](https://wiki.debian.org/AptCacherNg)  
[Setting up a cache server for apt packages (blog entry)](https://qmacro.org/blog/posts/2024/09/03/setting-up-a-cache-server-for-apt-packages/)

`/var/log/apt-cacher-ng/apt-cacher.log` - logi apt-cacher-ng

`tcpdump -i <iface> port 3142` - monitorowanie ruchu na porcie 3142


## Zadanie 2
`docker build . -t zad2:v0`

## Zadanie 3
`docker build . -t zad3:v0`

## Zadanie 4
`docker compose up -d`

## Zadanie 5
```dockerfile
FROM ubuntu:22.04
RUN apt-get update
RUN apt-get install -y curl

FROM alpine
ENV ADMIN_USER="mark"
RUN echo $ADMIN_USER > ./mark
RUN unset ADMIN_USER
```

### Możliwe nieoczekiwane zachowania i poprawki

- `apt-get update` i `apt-get install` w oddzielnych warstwach: `RUN apt-get update` tworzy warstwę z indeksem pakietów, kolejny `RUN apt-get install` w następnej warstwie może użyć przestarzałego indeksu  

- dwa różne `FROM` tworzą oddzielne build stages: warstwy z pierwszego `FROM ubuntu:22.04` nie przenoszą się automatycznie do obrazu opartego na `FROM alpine` Narzędzia z pierwszego obrazu nie będą dostępne w drugim  

- ENV vs RUN unset: `ENV ADMIN_USER="mark"` zapisuje zmienną w metadanych obrazu i przetrwa kolejne warstwy `RUN unset ADMIN_USER` usuwa ją tylko w tej powłoce, ale nie z metadanych obrazu (jeżeli zmienna ma być tylko podczas buildu lepiej korzystać `ARG` zamiast `ENV`)


## Zadanie 6


