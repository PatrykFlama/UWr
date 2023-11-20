[(wróć)](../)

# Lista 6
| 1 | 2 | 3 |
|---|---|---|
| X | ~ | X |


## Zadanie 2
1. generowanie klucza prywatnego  
`openssl genpkey -algorithm RSA -out key.pem -pkeyopt rsa_keygen_bits:2048`  
lub 
`openssl genrsa -out key.pem 2048`
2. generowanie certyfikatu  
`openssl req -new -x509 -key key.pem -out certificate.pem -days 365`
3. konwersja do PKCS#12 / PFX  
`openssl pkcs12 -export -out certificate.pfx -inkey key.pem -in certificate.pem`  
  
Mogą wystąpić problemy z plikiem .pfx w trakcie uruchamiania programu, z racji wykorzystania starego standardu RC2 do szyfrowania klucza prywatnego. Można obejść problem za pomocą flagi `--openssl-legacy-provider`
 <!-- lub naprawić plik .pfx za pomocą polecenia:
`openssl pkcs12 -in certificate.pfx -out certificate_fixed.pfx -nodes -passin pass: -passout pass:` -->


# Zadanie 3
`npm install express -g`  
`npm install ejs -g`  
`npm install nodemon -g` aby nie restartować manualnie serwera za każdym razem po zmianie pliku

