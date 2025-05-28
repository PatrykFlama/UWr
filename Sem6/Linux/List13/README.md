# Lista 13

| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
|---|---|---|---|---|---|---|---|---|----|
|   |   | X | X | X |   |   |   |   |    |

## Zadanie 1
```bash
sudo debootstrap bullseye /target/ http://deb.debian.org/debian
sudo unshare -imnpuf --mount-proc chroot /target/ /bin/bash
```
```bash
mount -t proc proc /proc
ps -ef
mount
ip link
```

## Zadanie 3
### systemd-cgls(1)
control group ls - pokazuje hierarchię grup kontrolnych (cgroups) w systemie  
- -M [MACHINE] - pokazuje hierarchię dla określonego kontenera

### systemd-cgtop(1)
control group top - monitoruje zużycie zasobów przez grupy kontrolne  
- -M [MACHINE] - pokazuje hierarchię dla określonego kontenera

## systemd-nspawn(1)
systemd-nspawn - narzędzie do uruchamiania kontenerów systemowych (chroot na sterydach)  
domyślnie izoluje przestrzenie nazw (pid, net, uts, ipc, mnt)

## systemd.resource-control(5)
systemd.resource-control - plik konfiguracyjny dla kontrolowania zasobów (per użytkownik?) w systemd  
(można ustawiać limity CPU `CPUQuota`, pamięci `MemoryLimit`, itp.)  
pliki konfiguracyjne znajdują się w `/{etc,lib,run}/systemd/system`



```bash
sudo systemd-nspawn -D /target
```

## Zadanie 4
- `lxc-create` - tworzy kontener
- `lxc-start` - uruchamia kontener
- `lxc-stop` - zatrzymuje kontener
- `lxc-attach` - pozwala wejść do działającego kontenera
- `lxc-console` - podłącza do wirtualnego terminala kontenera
- `lxc-ls` - listuje kontenery
- `lxc-info` - pokazuje status kontenera
- `lxc-monitor` - monitoruje zdarzenia związane z kontenerami

### tworeznie kontenera
```bash
sudo lxc-create -n guest1 -t download -- -d alpine -r edge -a amd64 --variant default
```

### konfiguracja sieci
w `/etc/network/interfaces`  
```bash
auto br0
iface br0 inet static
    address 10.0.1.1
    netmask 255.255.255.0
    bridge_ports none
    bridge_stp off
    bridge_fd 0
    bridge_maxwait 0
```

w `/var/lib/lxc/guest1/config`
```bash
lxc.net.0.type = veth
lxc.net.0.link = br0
lxc.net.0.flags = up
lxc.net.0.name = eth0
lxc.net.0.ipv4.address = 10.0.1.2/24
```

### łączenie z kontenerem

```bash
sudo lxc-start -n guest1
sudo lxc-attach -n guest1
```

### przestrzenie nazw
info o kontenerez (zawiera pid kontenera)  
```bash
sudo lxc-info -n guest1
```

przestrzenie nazw kontenera  
```bash
lsns | grep <PID_guest1>
```

cgroups
```bash
cat /proc/<PID_guest1>/cgroup
```


### automatyczne uruchamianie kontenera przy starcie systemu
```bash
sudo ln -s /var/lib/lxc/guest1 /etc/lxc/auto/guest1
```

właczanie usługi lxc (`lxc-autostart.service`)
```bash
sudo systemctl enable lxc
sudo systemctl start lxc
```

## Zadanie 5
### tworzymy użytkownika `vm`
```bash
sudo adduser vm
```

### tworzymy podgrupy/podużytkowników
w plikach `/etc/subuid` i `/etc/subgid` dodajemy `vm:<od>:<ile>`  
```bash
sudo nano /etc/subuid
sudo nano /etc/subgid

vm:100000:65536
```

### urządzenia sieciowe
aby zezwolić użytkownikowi `vm` na tworzenie wirtualnych interfejsów sieciowych, dodajemy wpis do pliku `/etc/lxc/lxc-usernet`  
```bash
echo "$(id -un) veth br0 10" | sudo tee -a /etc/lxc/lxc-usernet
```

### lxc config
tworzymy katalog `~/.config/lxc`  
kopiujemy `/etc/lxc/default.conf` do `~/.config/lxc/default.conf` lub korzystamy z poniższej templatki  
```bash
lxc.include = /etc/lxc/default.conf
lxc.idmap = u 0 100000 65536
lxc.idmap = g 0 100000 65536
lxc.net.0.type = veth
lxc.net.0.link = br0
lxc.net.0.flags = up
```

### tworzenie kontenera
```bash
lxc-create -n unguest -t download -- -d alpine -r edge -a amd64 --variant default
```

```bash
systemd-run --unit=my-unit --user --scope -p "Delegate=yes" -- lxc-start -n unguest
systemd-run --unit=my-unit --user --scope -p "Delegate=yes" -- lxc-attach -n unguest
```

### weryfikacja uid procesów i właściciea
```bash
# procesy w kontenerze
ps aux | grep unguest

# namespaces
lsns | grep $(pgrep -u vm lxc-start)

# monitor
pgrep -a -u vm lxc
```





# Some notes
## Zadanie 1
namespace pochodzą z `plan 9` (rozproszony system operacyjny), bo potrzebna była dobra separacja zasobów między procesami  
chcemy osiągnąć coś lepszego niż `chroot`   
`dbootstrap` tworzy minimalny system debiana (tak aby apt działał)  

> instalacja debiana wymaga odpalenia kilku skryptów w tworzonym systemie, co jest problematyczne gdy tworzym instalację na inną architekturę, niż ta z której korzsystamy  
> dalatego bootstrap wtedy jest 2-poziomowy

## Zadanie 2
teraz mamy ogranieczenie nie jakościowe  (namespaces), ale ilościowe  
cgroupy (informacje) znajdują się w `/sys/fs/cgroup`  
są w nim katalogi odpowiadające różnym grupom, a w tych katalogach pliki konfiguracyjne grupy  




