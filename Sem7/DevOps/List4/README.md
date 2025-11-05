# List 4
| 1 | 2 | 3 | 4 | 5 | 6 |
|---|---|---|---|---|---|
| x | x | x |   |   |   |


## Zad 1
Docker Swarm to natywny orkiestrator Dockera (coś w stylu Kubernetes) - pozwala na kontrolowanie wielu instancji silnika dockera.  

[docs](https://docs.docker.com/engine/swarm/)

```bash
docker swarm init --advertise-addr <IP_ADDRESS>

docker service create --name serv --replicas 3 nginx

docker service ls
docker service ps serv

docker service scale serv=5

docker swarm leave --force
```


## Zad 2
Docker network to narzędzie do zarządzania i wirtualizacji interfejsów sieciowych dla kontenerów Dockera.   

```bash
docker network create --driver bridge mynet

docker run -d --name ca --network mynet debian sleep 3600
docker run -d --name cb --network mynet debian sleep 3600

docker exec -it ca bash

ping -c 2 cb
```

odcinanie od internetu:

```bash
docker network create --driver bridge --internal netless
docker run -d --name cc --network netless debian sleep 3600
```


## Zad 3
- unshare - uruchamia proces w nowych namespace'ach  
- nsenter - uruchamia proces w istniejących namespace'ach procesu docelowego  

separacja przestrzeni PID:
- `--pid` - nowa przestrzeń PID
- `--fork` - forkuje nowy proces jako dziecko naszego
- `--mount-proc` - montuje /proc dla nowej przestrzeni PID
```bash
sudo unshare --pid --fork --mount-proc bash
ps aux
```

wchodzenie do namespace innego procesu:
- `t` - target PID
- `a` - wejdź we wszystkie namespace'y
```bash
docker run -d --name demo debian sleep 3600
docker inspect -f '{{.State.Pid}}' demo
sudo nsenter -t <PID> -a -- /bin/bash
```

firejail - narzędzie do uruchamiania procesów w odizolowanych środowiskach  
uruchamianie basha w odizolowanym środowisku:
- `---private` - system plików jest odizolowany i tymczasowy
- `--private=DIR` - tworzy tymczasowy katalog domowy
```bash
sudo firejail --private=/tmp bash -c "sleep 3600 &"
sudo nsenter -t <PID> -a -- /bin/bash
```

