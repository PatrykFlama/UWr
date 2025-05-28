# Lista 13

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
```bash
sudo ip link add name br0 type bridge
sudo ip addr add 10.0.1.1/24 dev br0
sudo ip link set br0 up
```

lub netplan w `/etc/netplan/01-lxcbridge.yaml`
```yaml
network:
  version: 2
  renderer: networkd
  ethernets:
    ens18: {}
  bridges:
    br0:
      interfaces: [ens18]
      dhcp4: yes
      addresses: [10.0.1.1/24]
      parameters:
        stp: false
        forward-delay: 0
```

w `/etc/lxc/guest1.conf`
```bash
lxc.net.0.type = veth
lxc.net.0.link = br0
lxc.net.0.flags = up
lxc.net.0.name = eth0
lxc.net.0.hwaddr = 00:16:3e:xx:xx:xx
```

w `/var/lib/lxc/guest1/rootfs/etc/network/interfaces`
```bash
auto eth0
iface eth0 inet static
    address 10.0.1.2
    netmask 255.255.255.0
    gateway 10.0.1.1
```

### łączenie z kontenerem

```bash
sudo lxc-start -n guest1
ssh root@10.0.1.2
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

