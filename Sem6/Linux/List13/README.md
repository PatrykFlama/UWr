# Lista 13

| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
|---|---|---|---|---|---|---|---|---|----|
|   |   |   | X |   |   |   |   |   |    |

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

