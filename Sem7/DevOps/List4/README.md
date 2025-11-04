# List 4
| 1 | 2 | 3 | 4 | 5 | 6 |
|---|---|---|---|---|---|
| x | x |   |   |   |   |


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




