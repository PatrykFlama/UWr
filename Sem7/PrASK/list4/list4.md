## instalacja openvpn
instalujemy [`openvpn`](https://openvpn.net/community-docs/how-to.html) oraz rekomendowaną paczkę [`easy-rsa`](https://github.com/OpenVPN/easy-rsa)

```bash
sudo apt update
sudo apt install openvpn easy-rsa
```

## konfiguracja openvpn jako serwer
### autoryzacja po kluczach ssl (nowe CA)
tworzymy nowe CA dla naszego openvpn (skorzystamy z easy-rsa):
```bash
make-cadir ~/openvpn-ca

./easyrsa init-pki
./easyrsa build-ca
```

następnie konfigurujemy serwer openvpn w `/etc/openvpn/server.conf` (możemy skorzystać z przykładowego pliku z `/usr/share/doc/openvpn/examples`):
```conf
port 1194
proto udp
dev tun

ca /etc/openvpn/server/ca.crt
cert /etc/openvpn/server/server.crt
key /etc/openvpn/server/server.key
dh /etc/openvpn/server/dh.pem

topology subnet

ifconfig-pool-persist /var/log/openvpn/ipp.txt

keepalive 10 120
persist-key
persist-tun

status /var/log/openvpn-status.log
verb 3

explicit-exit-notify 1
```

teraz tworzymy dh oraz certyfikaty dla serwera i klienta (za pomocą easyrsa):
```bash
./easyrsa gen-dh
./easyrsa build-server-full server nopass
./easyrsa build-client-full client1 nopass

sudo cp pki/ca.crt /etc/openvpn/server
sudo cp pki/dh.pem /etc/openvpn/server
sudo cp pki/issued/server.crt /etc/openvpn/server
sudo cp pki/private/server.key /etc/openvpn/server
```

na koniec, aby przetestować konfiguracę, startujemy server z folderu `/etc/openvpn/server` i jeżeli nie będzie błędów to startujemy jako daemon:
```bash
sudo openvpn /etc/openvpn/server/server.conf

sudo systemctl enable openvpn@server
sudo systemctl start openvpn@server
```


## konfiguracja z autoryzacją PAM
tworzymy analogiczny config do poprzedniego (`/etc/openvpn/server-pam.conf`):
```conf
port 1195
proto udp
dev tun1

ca /etc/openvpn/server/ca.crt
cert /etc/openvpn/server/server.crt
key /etc/openvpn/server/server.key
dh /etc/openvpn/server/dh.pem

plugin /usr/lib/openvpn/openvpn-plugin-auth-pam.so login
verify-client-cert none
username-as-common-name
```


## konfiguracja klienta
```conf
client
dev tun
proto udp
remote 91.204.161.220 1194

resolv-retry infinite
nobind

persist-key
persist-tun

ca ca.crt
cert client1.crt
key client1.key

remote-cert-tls server
verb 3
```

```bash
sudo openvpn --config client.conf
```

aby vpn startował bo boocie, konfigurujemy go jako daemona tak samo jak serewr

![alt text](image.png)

## instalacja wireguard
instalujemy [wireguard'a](https://www.wireguard.com/quickstart/)
```bash
sudo apt update
sudo apt install wireguard
```



