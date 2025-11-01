# Lista 2
## Instalacja serwera
zainstalujemy nginx:  
`nginx openssl certbot python3-certbot-nginx`

> serwer powinien automatycznie wystartować, ale warto się upewnić (`nginx.service`)

po wystartowaniu serwera, powinniśmy być w stanie wejść na główną stronę serwera i zobaczyć stronę powitalną (a dowolne inne złe ścieżki otrzymają stronę 404 od nginx)

do konfiguracji interesować będzie nas plik `/etc/nginx/nginx.conf`

## Umieszczanie ciągu znaków oraz odznaki na stronie
### Tworzenie struktury statycznie serwowanej strnoy
będziemy serwować statyczne pliki, więc w katalogu `/var/www/` tworzymy katalog dla naszej strony (np `patrykflama`, stąd będziemy serwować pliki)  
zmieniamy też właściciela tego katalogu na naszego użytkownika (`chown`)  

### Umieszczanie ciągu znaków w base64 na stronie
Kodujemy ciąg znaków za pomocą programu `base64` oraz zapisujemy w głównym folderze strony jako `ala.txt`. Zapisujemy w nim też odznakę `badge.png`. Następnie tworzymy `index.html`:  

```html
<html><body>
<h1>patrykflama.work.gd</h1>

<p>Base64:
<object data="/ala.txt" type="text/plain">
<a href="/ala.txt">No support?</a>
</object>
</p>

<img src="https://ipv6.he.net/certification/create_badge.php?pass_name=patrykflama&badge=1" alt="list 1 badge">

</body></html>
```

### Konfigurowanie strony w nginx
możemy potraktować jako wzór automatycznie utworzony plik `/etc/nignx/sites-available/default` - w tym folderze deklarujemy nasze strony
natomiast w folderze `sites-enabled` tworzymy link symboliczny do naszej konfiguracji, jeżeli chcemy ją 'włączyć'  
za pomocą flagi `nginx -t` możemy sprawdzić poprawność naszej konfiguracji  
na koniec restartujemy daemona nginx

przykładowa konfiguracja dla naszej strony:
```config
server {
    listen 80;
    server_name patrykflama.work.gd www.patrykflama.work.gd;
    root /var/www/patrykflama/root;
    index index.html;
}
```

## Generowanie certyfikatu self-signed
generujemy klucz oraz certyfikat za pomocą `openssl-req(1)`:

```bash
sudo openssl req -x509 -nodes -days 365 -newkey rsa:4096 \
  -subj "/CN=www1.patrykflama.work.gd/O=patrykflama" \
  -keyout /etc/ssl/private/www1.key \
  -out /etc/ssl/certs/www1.crt
```

oraz dodajemy je do naszej konfiguracji w nginx:  
```conf
ssl_certificate /etc/ssl/certs/www1.crt;
ssl_certificate_key /etc/ssl/private/www1.key;

include /etc/nginx/snippets/ssl-params.conf;
```

Z punktu widzenia klienta, taki certyfikat nie jest dobry - bo nie jest podpisany przez zaufane CA (Certificate Authority), więc przeglądarka wyświetli błąd o niezaufanym certyfikacie, a użytkownik nie ma gwarancji że widzi stronę prawdziwego właściciela domeny.


## Tworzenie własnego ośrodka certyfikacji (CA)
najpierw musimy stworzyć klucz prywatny dla naszego ośrodka, a następnie podpisany przez niego certyfikat:  
```bash
sudo openssl genrsa -out /etc/ssl/myca/private/ca.key.pem 4096
sudo openssl req -x509 -new -nodes -key /etc/ssl/myca/private/ca.key.pem \
  -sha256 -days 3650 -out /etc/ssl/myca/certs/ca.cert.pem \
  -subj "/C=PL/ST=PL/L=PL/O=PatrykFlama/CN=PatrykFalma"
```

dodawanie naszego CA do zaufanych (na lokalnym komputerze)
```bash
sudo apt install -y ca-certificates
sudo cp $CANAME.crt /usr/local/share/ca-certificates
sudo update-ca-certificates
```

## Certyfikat naszej strony od naszego CA
generujemy certyfikat dla naszej strony
```bash
sudo openssl genrsa -out /etc/ssl/patrykflama/www2.key 2048
sudo openssl req -new -key /etc/ssl/patrykflama/www2.key -out /tmp/www2.csr \
  -subj "/CN=www2.patrykflama.work.gd/O=PatrykFlama"
```

i go podpisujemy

```bash
echo > www2_v3.ext << EOF
authorityKeyIdentifier=keyid,issuer
basicConstraints = CA:true
keyUsage = digitalSignature, keyEncipherment
extendedKeyUsage = serverAuth
subjectAltName = DNS:www2.patrykflama.work.gd
EOF

sudo openssl x509 -req -in /tmp/www2.csr -CA /etc/ssl/myca/certs/ca.cert.pem \
  -CAkey /etc/ssl/myca/private/ca.key.pem -CAcreateserial \
  -out /etc/ssl/patrykflama/www2.crt -days 825 -sha256 -extfile www2_v3.ext
```

na koniec podpinamy certyfikat pod www2

```conf
server {
  listen 443 ssl;
  server_name www2.patrykflama.work.gd
  root /var/www/patrykflama/;

  ssl_certificate /etc/ssl/patrykflama/www2.crt;
  ssl_certificate_key /etc/ssl/patrykflama/www2.key;
  ssl_trusted_certificate /etc/ssl/myca/certs/ca.cert.pem;

  include /etc/nginx/snippets/ssl-params.conf;
}
```

## Certyfikat wildcard
teraz `wild_v3.ext`:
```bash
echo > wild_v3.ext << EOF
authorityKeyIdentifier=keyid,issuer
basicConstraints=CA:TRUE
keyUsage = digitalSignature, keyEncipherment
extendedKeyUsage = serverAuth
subjectAltName = DNS:*.patrykflama.work.gd, DNS:patrykflama.work.gd
EOF
```

oraz generujemy klucze
```bash
sudo openssl genrsa -out /etc/ssl/patrykflama/wild.key 4096
sudo openssl req -new -key /etc/ssl/patrykflama/wild.key -out /tmp/wild.csr \
  -subj "/CN=*.patrykflama.work.gd/O=PatrykFlama_wildcard"
sudo openssl x509 -req -in /tmp/wild.csr -CA /etc/ssl/myca/certs/ca.cert.pem \
  -CAkey /etc/ssl/myca/private/ca.key.pem -CAcreateserial \
  -out /etc/ssl/patrykflama/wild.crt -days 825 -sha256 -extfile wild_v3.ext
```



