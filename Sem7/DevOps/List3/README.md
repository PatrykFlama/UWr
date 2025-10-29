[(back)](../)

# Lista 3
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|---|---|---|---|---|---|---|---|
| x | x | x | x |   | x |   | x |


## Zad 1 i 8
Lista 2 zad 4 i 8

## Zad 2
Użytkownik w grupie `docker` ma uprawnienia `root` względem hosta (docker działa jako root). Możemy więc utworzyć kontener z dostępem do całego systemu plików i przeczytać falgę.

```bash
docker run -it --rm -v ./:/mnt debian
```

## Zad 3
Mamy 2 wersje jakiejś aplikacjie, obie korzystające z /root/katalog (zamontowanego z lista3zA)  
odpalając nową wersję, mogła ona zepsuć dane w tym miejscu - wiec obie wersje już nie działają   

problem z wolumenami: tracimy reprodukowalność  
jak sobie radzić z tym problemem? trzymać backup danych przed zaktualizowaniem

## Zad 4
- sprawdzanie wersji cgroups:
```bash
stat -fc %T /sys/fs/cgroup
mount | grep cgroup
```
(jeśli `cgroup2fs`, to v2, jeśli `tmpfs`, to v1)


> ```bash
> /proc/self/status | grep PPid
> ```


- sprawdzanie cgroup procesu:
```bash
cat /proc/<proces>/cgroup
cat /proc/$$/cgroup
```

- sprawdzanie ograniczeń
```bash
# co ta grupa może ograniczać
cat /sys/fs/cgroup/<cgroupa>/cgroup.controllers

# nałożone ograniczenia
cat /sys/fs/cgroup/<cgroupa>/memory.max
cat /sys/fs/cgroup/<cgroupa>/cpu.max
```


- tworzenie własnej cgroupy
```bash
sudo mkdir /sys/fs/cgroup/demo
echo $$ | sudo tee /sys/fs/cgroup/demo/cgroup.procs
echo 100 | sudo tee /sys/fs/cgroup/demo/cpu.max

# lub
sudo mkdidr /sys/fs/cgroup/user.slice/demo   # pliki ustawień same się utowrzą
echo 1 | sudo tee /sys/fs/cgroup/user.slice/demo/pids.max   # ustawienie limitu procesów na 1
echo $$ | sudo tee /sys/fs/cgroup/user.slice/demo/cgroup.procs   # dodanie bieżącego procesu do cgroupy
# teraz praktycznie nie możemy nic zrobić, bo nasz terminal zużywa wszystkie procesy
```

- cgroup.stat - to są kontrolery z kolejnego zadania, ale ciężko o tym w stanie znaleźć informacje
  - nr_descendants 0 - liczba potomków tej cgroup (bez bieżącej)
  - nr_subsys_cpu 1 - ile cpu używa ta cgroupa


## Zad 5
`cgroups(7)`  

- net_prio - w danej cgroupie dodajemy priorytety w sieci konkretnym procesom
- net_cls - klasyfikacja pakietów sieciowych, aplikowane do pakietów wychodzących z procesów w danej cgroupie
- devices - kontrola dostępu do urządzeń (czytanie/pisanie)
- freezer - zamrażanie i odmrażanie procesów w cgroupie (przydatne do robienia snapshotów)


## Zad 6
`systemd.slice(5)`  

"slice to logiczna grupa procesów zarządzana przez systemd, odpowiadająca cgroup" - taki odpowiednik cgroup w systemd, warstwa abstrakcji nad cgroupami  
można do nich dodawać procesy i nakładać ograniczenia zasobów

- tworzenie slice:
```bash
sudo systemd-run --unit=myservice1.service --slice=myslice.slice sleep 600
sudo systemd-run --unit=myservice2.service --slice=myslice.slice yes > /dev/null
```

- nakładanie ograniczeń:
```bash
sudo systemctl set-property myslice.slice MemoryMax=100M CPUQuota=20%
```

- sprawdzanie stanu slice:
```bash
systemd-cgtop # aktualne zużycie zasobów
systemd-cgls  # rekurencyjnie pokazuje zawartość cgroup
```



## Zad 7
- soft-limit - zawsze chcemy mieć dostęp do tych zasobów, ale w razie potrzeby system może nam je odebrać    
- hard-limit - nigdy nie możemy przekroczyć tych zasobów, system będzie blokował próby ich przekroczenia  

