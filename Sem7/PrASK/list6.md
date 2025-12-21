# Lista 6

instalujemy dockera wg instrukcji na stronie


tworzymy sieć oraz kontenery:
```bash
docker network create --subnet 10.0.0.0/24 --gateway 10.0.0.254 --driver bridge prasknet

docker run --cap-add=NET_ADMIN --name vm1 --network=prasknet --ip=10.0.0.1 -dv /srv/www/vm1:/usr/share/nginx/html:ro nginx

docker run --cap-add=NET_ADMIN --name vm2 --network=prasknet --ip=10.0.0.2 -dv /srv/www/vm2:/usr/share/nginx/html:ro nginx
```


konfiguracja ipv6: https://docs.docker.com/engine/daemon/ipv6/


konfigurujemy główną maszynę:
```bash
nft add table nat && nft 'add chain nat postrouting { type nat hook postrouting priority 100 ; }' && nft add rule nat postrouting ip saddr 10.0.0.0/24 oif ens18 snat to 91.204.161.220

echo 1 > /proc/sys/net/ipv4/ip_forward
````

> dla wygody zmiany konfiguracji, korzystałem z docker compose, oto wynikowy plik:

```yaml
services:
  vm1:
    image: nginx:latest
    container_name: vm1
    cap_add:
      - NET_ADMIN
    networks:
      prasknet:
        ipv4_address: 10.0.0.1
        ipv6_address: 2a0b:5485:1:18::a
    volumes:
      - /srv/www/vm1:/usr/share/nginx/html:ro

  vm2:
    image: nginx:latest
    container_name: vm2
    cap_add:
      - NET_ADMIN
    networks:
      prasknet:
        ipv4_address: 10.0.0.2
        ipv6_address: 2a0b:5485:1:18::b
    volumes:
      - /srv/www/vm2:/usr/share/nginx/html:ro

networks:
  prasknet:
    driver: bridge
    enable_ipv6: true
    ipam:
      config:
        - subnet: 10.0.0.0/24
          gateway: 10.0.0.254
        - subnet: 2a0b:5485:1:18::/64
```



teraz musimy znaleźć interfejs sieci prasknet:
```bash
ip l show type bridge
```
szukamy interfejsu odpowiadającemu naszemu bridge (`br-...`)
i nadajemy mu IPv4 10.0.0.254/24, IPv6 fe80::1

```bash
sudo ip addr add 10.0.0.254/24 dev br-32b7a0af79fd
sudo ip -6 addr add fe80::1/64 dev br-32b7a0af79fd

ip addr show dev br-32b7a0af79fd

sudo ip link set br-32b7a0af79fd up
```


testujemy ping:
```bash
ping 10.0.0.1
ping 10.0.0.2

# wygenerowany adres ipv6 bierzemy z vm1
ping -6 fe80::941e:deff:fefc:aa07%br-32b7a0af79fd
```




w obu maszynach dodajemy w `/usr/share/nginx/html/index.html` 'identyfikację' - czyli w `/srv/www/vm*`


konfigurujemy round robin oraz failover na głównym nginx

```conf
upstream backend {
    server 10.0.0.1:80 max_fails=2 fail_timeout=2s;
    server 10.0.0.2:80 max_fails=2 fail_timeout=2s;
}

server {
    server_name proxy.patrykflama.dev;

    listen [::]:443 ssl ipv6only=on; # managed by Certbot
    listen 443 ssl; # managed by Certbot

    location / {
        proxy_pass http://backend;
    }
}
```


i testujemy:
```bash
for i in {1..10} ;
do 
    curl https://proxy.patrykflama.dev
done
```



instalujemy keepalived na obu kontenerach  
```bash
docker exec -it vm1 apt install -y keepalived
docker exec -it vm2 apt install -y keepalived
```


tworzymy skrypt spardzający czy nginx żyje
```bash
pidof nginx >/dev/null
```

konfiguracje keepalived   
vm1:  
```conf
global_defs {
    enable_script_security
    script_user root
}

vrrp_script chk_nginx {
    script "/usr/local/bin/check_nginx.sh"
    interval 1
    fall 2
    rise 1
}

vrrp_instance VI_4 {
    state MASTER
    interface eth0
    virtual_router_id 42
    priority 200
    advert_int 1

    track_script {
        chk_nginx
    }

    virtual_ipaddress {
        10.0.0.12/24
    }
}

vrrp_instance VI_6 {
    state MASTER
    interface eth0
    virtual_router_id 43
    priority 200
    advert_int 1

    track_script {
        chk_nginx
    }

    virtual_ipaddress {
        2a0b:5485:1:18::ab/64
    }
}
```


```conf
global_defs {
    enable_script_security
    script_user root
}

vrrp_script chk_nginx {
    script "/usr/local/bin/check_nginx.sh"
    interval 1
    fall 2
    rise 1
}

vrrp_instance VI_4 {
    state BACKUP
    interface eth0
    virtual_router_id 42
    priority 100
    advert_int 1

    track_script {
        chk_nginx
    }

    virtual_ipaddress {
        10.0.0.12/24
    }
}

vrrp_instance VI_6 {
    state BACKUP
    interface eth0
    virtual_router_id 43
    priority 100
    advert_int 1

    track_script {
        chk_nginx
    }

    virtual_ipaddress {
        2a0b:5485:1:18::ab/64
    }
}
```



sprawdzamy konfigurację i odpalamy keepalived
```bash
keepalived -t
keepalived -n -l

# za pomocą systemd
systemctl enable keepalived
systemctl start keepalived
```












