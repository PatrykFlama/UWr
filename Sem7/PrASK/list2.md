# Zad 1
## Instalacja serwera
zainstalujemy nginx:  
`nginx openssl`

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

## Generowanie certyfikatu self-signed - www1
generujemy klucz oraz certyfikat za pomocą `openssl-req(1)`:

```bash
sudo openssl req -x509 -nodes -days 365 -newkey rsa:4096 \
  -subj "/CN=www1.patrykflama.work.gd/O=patrykflama" \
  -addext "subjectAltName=DNS:www1.patrykflama.work.gd" \
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

## Certyfikat naszej strony od naszego CA - www2
generujemy certyfikat dla naszej strony
```bash
sudo openssl genrsa -out /etc/ssl/patrykflama/www2.key 2048
sudo openssl req -new -key /etc/ssl/patrykflama/www2.key -out /tmp/www2.csr \
  -subj "/CN=www2.patrykflama.work.gd/O=PatrykFlama"
```

i go podpisujemy

```bash
cat << EOF > www2_v3.ext
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

## Certyfikat wildcard - www3
teraz `wild_v3.ext`:
```bash
cat <<EOF > wild_v3.ext
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
  -subj "/CN=*.patrykflama.work.gd/O=PatrykFlama_wildcard" \

sudo openssl x509 -req -in /tmp/wild.csr -CA /etc/ssl/myca/certs/ca.cert.pem \
  -CAkey /etc/ssl/myca/private/ca.key.pem -CAcreateserial \
  -out /etc/ssl/patrykflama/wild.crt -days 825 -sha256 -extfile wild_v3.ext
```

## Lets Encrypt
instalujemy certbota (klient letsencrypt):
```bash
sudo apt update && sudo apt install certbot python3-certbot-nginx -y
```

konfigurowanie certyfikatu dla `www`:
```bash
sudo certbot --nginx -d www.patrykflama.work.gd
```

> certbot automatycznie skonfiguruje deamona do automatycznego odnawiania certyfikatu

```
Requesting a certificate for www.patrykflama.work.gd

Successfully received certificate.
Certificate is saved at: /etc/letsencrypt/live/www.patrykflama.work.gd/fullchain.pem
Key is saved at:         /etc/letsencrypt/live/www.patrykflama.work.gd/privkey.pem
This certificate expires on 2026-01-30.
These files will be updated when the certificate renews.
Certbot has set up a scheduled task to automatically renew this certificate in the background.

Deploying certificate
Successfully deployed certificate for www.patrykflama.work.gd to /etc/nginx/sites-enabled/patrykflama
Congratulations! You have successfully enabled HTTPS on https://www.patrykflama.work.gd
```




# Zad 2
generujemy dhparam
```bash
sudo openssl dhparam -out /etc/letsencrypt/dhparam.pem 2048
```

i korzystamy z niego w konfiguracji ssl (`ssl-params.conf`):
```conf 
# from https://gist.github.com/ziazek/ae2cb56fe63f8727dbaa55cddbc9780e

ssl_protocols TLSv1.2;
ssl_prefer_server_ciphers on;
ssl_ciphers "EECDH+AESGCM:EDH+AESGCM:AES256+EECDH:AES256+EDH";
ssl_ecdh_curve secp384r1;
ssl_session_cache shared:SSL:10m;
ssl_session_tickets off;
ssl_stapling on;
ssl_stapling_verify on;
# use Google DNS
resolver 8.8.8.8 8.8.4.4 valid=300s;
resolver_timeout 5s;

# HSTS header: BE CAREFUL!
# Uncommenting this setting means that your site needs to support HTTPS in the future (including subdomains),
# otherwise users who have previously been to your site won't be able to access.
# add_header Strict-Transport-Security "max-age=63072000; includeSubDomains; preload";

add_header X-Frame-Options DENY;
add_header X-Content-Type-Options nosniff;

ssl_dhparam /etc/letsencrypt/dhparam.pem;
```


____

- HSTS (HTTP Strict Transport Security)  
Meachanizm wymuszający korzystanie z HTTPS  
Gdy przeglądarka po raz pierwszy odwiedzi stronę z nagłówkiem `Strict-Transport-Security: max-age=31536000; includeSubDomains; preload` zapamiętuje, że dana domena musi być odwiedzana tylko przez HTTPS (chroni to przed atakami downgrade, które próbują wymusić połączenie HTTP)

- PFS / FS (Perfect Forward Secrecy / Forward Secrecy)  
Zasada kryptograficzna zapewniająca, że nawet jeśli klucz prywatny serwera zostanie kiedyś skradziony, stare sesje nie mogą być odszyfrowane - dla każdej sesji generowane są tymczasowe klucze.


- ALPN (Application-Layer Protocol Negotiation)  
Rozszerzenie TLS pozwalające klientowi i serwerowi dogadać się, jaki protokół warstwy aplikacji będą używać w ramach handshake TLS (np. HTTP/1.1, HTTP/2, HTTP/3) -
dzięki temu nie trzeba wykonywać dodatkowych zapytań po nawiązaniu TLS

- NPN (Next Protocol Negotiation)  
Poprzednik ALPN - opracowany przez Google do obsługi SPDY i wczesnych wersji HTTP/2.

- CAA (Certification Authority Authorization)  
Rekord DNS, który określa które urzędy certyfikacji mogą wystawiać certyfikaty dla danej domeny.
Chroni przed nieautoryzowanym wystawieniem certyfikatu (gdyby inne CA zostało skompromitowane).

- OCSP (Online Certificate Status Protocol)  
Protokół służący do sprawdzania, czy certyfikat SSL jest nadal ważny, czy został unieważniony przez CA (przeglądarka pyta CA czy certyfikat jest ważny)


