# lista 2 / list no. 2
1. Zainstaluj na swojej maszynie wirtualnej dowolny serwer http. Serwer musi obsługiwać vhosty.

Za pomocą pakietu openssl (libressl/polarssl/*ssl):

a) Zakoduj ciąg znaków “ala ma kota” za pomocą base64 i umieść go na stronie głównej serwera.
Na swojej stronie umieść również swoją “odznakę” z zad1, lista1 (1p)

b) Wygeneruj certyfikat self-signed dla domeny www1.<twojadomena.tld.>
Opisz dlaczego taki certyfikat nie jest dobrym certyfikatem (z punktu widzenia klienta). (1p)

c) Stwórz ośrodek certyfikacji (CA). Możesz spróbować zainstalować swój certyfikat CA na komputerze. (2p)
(pamiętaj, że certyfikaty CA powinny muszą mieć odpowiednie atrybuty, np. basicConstraints = CA:true)

d) Wygeneruj żądanie certyfikacji ssl (CSR) dla domeny www2.<twojadomena.tld.> następnie podpisz je certyfikatem swojego CA. (2p)
(wygenerowany certyfikat powinien mieć typ “server”)

e) Wygeneruj żądanie certyfikacji (CSR) na wszystkie adresy z TLD Twojej domeny (np. *.tld) i podpisz je certyfikatem swojego CA. (1p)
(wygenerowany certyfikat powinien mieć typ “server”, w zadaniu chodzi wyłącznie o certyfikat wildcard)

f) Zapoznaj się z inicjatywą LetsEncrypt (https://letsencrypt.org/).
Wygeneruj certyfikat dla swojej domeny oraz www.<twojadomena.tld.> (2p)

Wygenerowane certyfikaty umieść pod odpowiednimi vhostami (np. self-signed pod www1.<twojadomena.tld.>). Docelowa struktura:

```
https://www1.twojadomena.tld -> self-signed
https://www2.twojadomena.tld -> podpisane własnym CA
https://www3.twojadomena.tld -> dla całego *.tld, podpisane własnym CA
https://twojadomena.tld oraz https://www.twojadomena.tld -> LetsEncrypt
```

```
Wejdź przeglądarką na każdą z tych stron i wytłumacz w kilku słowach jej zachowanie.
```

```
Odwiedź stronę https://www.ssllabs.com/ssltest/analyze.html i przeanalizuj wyniki dla swoich stron oraz dla dowolnie wybranych witryn.
```

> Dopisek na prośbę: Należy oczywiście zamienić <twojadomena.tld.> na nazwę własnej domeny.
2. Doprowadź by analizator SSL ze strony https://www.ssllabs.com/ssltest/analyze.html pokazywał co najmniej wynik “A” dla Twojej strony.
Wytłumacz czym jest HSTS, PFS (lub FS), ALPN, CAA, OCSP. (2p)

```
 Dla chętnych: poczytaj również o BEAST, POODLE, Heartbleed, CCS, HPKP (już nie używany).
```
3. Odwiedź stronę https://securityheaders.io/ i tak ustaw nagłówki http, żeby uzyskać wynik A+ dla swojej strony. Wytłumacz czym jest XSS i CSP. (2p)

```
 Dla chętnych: sprawdź inne znane strony w/w analizatorami. Np. strony znanych banków. Wyciągnij wnioski.
```

4. Skonfiguruj HSTS oraz CAA dla swojego serwera - dotyczy www.twojadomena.tld oraz twojadomena.tld i certyfikatów wydanych przez LetsEncrypt. Zweryfikuj ponownie wynik analizatorem z zad. 2. (2p)

