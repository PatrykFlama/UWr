## instalacja openvpn
instalujemy `openvpn` oraz rekomendowaną paczkę [`easy-rsa`](https://github.com/OpenVPN/easy-rsa)

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

server 10.8.0.0 255.255.255.0
push "redirect-gateway def1"
push "dhcp-option DNS 1.1.1.1"

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


