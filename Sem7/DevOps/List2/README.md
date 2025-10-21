# List 2
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
|---|---|---|---|---|---|---|---|---|----|
| x | x | x | x | x |   | x | x | x | x  |


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

## Zadanie 7
[Resource constraints docs](https://docs.docker.com/engine/containers/resource_constraints/#cpu) - 
nakłada ograniczenie na używane procesory oraz ich rdzenie  
(po co? obv)

[Docker build cache](https://docs.docker.com/build/cache/)  
Cache budowania obrazów zapisuje każdą warstwę jako osobny obraz - więc jeżeli zmienimy instrukcję budowania warstwy, to docker zaznaczy że jej obraz nie jest już aktualny     
- `docker image ls --all`
wypisuje wszystkie obrazy  

- `docker image history <obraz>`
wypisuje historię warstw obrazu (przez jaką instrukcję została stworzona oraz id tej wersji)

- `docker image inspect <obraz>`
wypisuje metadane obrazu


## Zadanie 8
[Docker bake docs](https://docs.docker.com/build/bake/) - abstrakcja do `docker build`.  
Bake to plik do **deklaratywnego** budowania **wielu obrazów** jednocześnie. Dzięki temu, jeżeli nasza aplikacja składa się z wielu obrazów (np frontend, backend, baza danych), to możemy je zbudować jednym poleceniem (oraz deklaratywnie).  

po co?  
- deklaratywne
- wiele obrazów na raz
- np gdy budujemy na wiele platform (amd64, arm64)

`docker buildx bake`

```h
target "myapp" {
  context = "."
  dockerfile = "Dockerfile"
  tags = ["myapp:latest"]
  args = {
    foo = "bar"
  }
  no-cache = true
  platforms = ["linux/amd64", "linux/arm64"]
}
```


## Zadanie 9
- katalog w home stworzony jako root - użytkownik nie ma do niego uprawnień
- CMD uruchamiane w katalogu `/`
- `JSON arguments recommended for ENTRYPOINT/CMD to prevent unintended behavior related to OS signals (JSON arguments recommended for CMD to prevent unintended behavior related to OS signals)Docker DX (docker-language-server)JSONArgsRecommended`

## Zadanie 10
optymalizacja rozmiaru obrazu:
- czyszczenie cache apt: `RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*`
- mniejszy obraz: np `alpine`
- budowanie aplikacji w dużym obrazie, a uruchamianie w osobnym, tylko kompilatów

