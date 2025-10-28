# Lecture 1 - atak wykorzystujący napisy do filmów 
Paper: Hacked in Translation

## atak XSS (cross site scripting) 
w html mamy boxa który interpretuje input jako html (np żeby dodać kursywę `<i>kursywa</i>`)  
przykład: strona pobiera arguemnty z linku na który weszliśmy, atakujący podsyła link ofiarze podając jako arguemnt wartość `<script>wyślij ciasteczka do atakującego</script>` - to np klucz sesji zostanie przesłany do atakującego  


> jak się przed czymś takim bronić (jako programista)  
> łatanie problemów nie jest specjalnie efektywne (bo mogą istanieć/powstać inne)  
> lepiej skorzystać z:  
> - deserializacji  
> - sanityzacji 


> mXSS - mutation XSS, technika bazująca na tym że coś jest serializowane (tylko wtedy ona działa) 


# Lecture 2 - nix i nixos
fajny bo reprodukowalny, deklaratywny  
dodatkowo manager pakietów nix może być używany np na archu  

`nix-shell -p <paczka>` - uruchamiamy powłokę z podanymi paczkami (po wyjściu z powłoki już ich nie ma)  

pakiety są w `/nix/store/<hash wyliczony z wersji pakietu i zaleśności>-<pakiet>-<wersja>/`

`nix-shell -p <pakiet> -I <link do nixpkgs>` - używamy innego repozytorium pakietów niż domyślne

możemy użyć nix-shell jako interpretera skryptów

## język nix
podstawy: 
- arytmetyka `21 - 1`
- zmienne `x = 1` `x - 1`
- listy `[ 1 2 3 ]`
- obiekty `{ a = 1; b = 2; }`
  - `x.a + x.b`
  - `with x; a + b`
- funkcje `f = x: x + 1`
  - `f 2`
  - `f = { a, b }: a + b`
  - `f { a = 1; b = 2; }`


wywołanie powłoki `nix-shell` domyślnie szuka pliku `.nix` w bieżącym katalogu  


`nix-env -g` - zainstalowane pakiety
`nix-env --list-generations` - generacje (punkty przywracania)
`nix-env --rollback` - przywrócenie poprzedniej generacji
`nix-env -G <numer generacji>` - przywrócenie do podanej generacji 

`nix-store` - zarządzanie magazynem nix

## nixos



# Lecture 3
## DNS
> uwaga na cloudflare jako dns provider, bo business plan jest drogi  

jak bezpiecznie przeprowadzić wymianę nameserwerów? jak mamy serwery primary i secondary (master i slave) to najpierw zmieniamy secondary, który to powinien wykryć i zapytać nowy serwer o konfigurację  

TSIG:   
jak zwiększyć bezpieczeństwo? korzystamy z klucza symetrycznego TSIG na obu serwerach (primary, sercondary) - będą one wymagać potwierdzenia tożsamości, więc tylko serwery z kluczem będą mogły się synchronizować (zapytanie o konfigurację i odpowiedź są podpisywane/szyfrowane nim) - pytanie tylko jak dystrybuować klucz?  


DNSSEC:  
informacje o dns nie są w żaden sposób weryfikowane - np możemy sobie postawić serwer który wskazuje na google.com  

> niby nikt nie będzie wskazywać na nasz serwer, ale np kraj może wymusić korzystanie z jednego serwera (taka forma inwigilacji)  

> duzi dostawcy internetu w polsce mają blacklisty stron hazardowych i przekazują do strony informacyjnej że strona jest zablokowana  

wymyślono więc DNSSEC, żeby chroniż zapytania DNS - zaczyna się on od root serwerów (które mają swoje klucze, trzymane również bezpiecznie w sejfach, są one też regularnie rotowane - co może powodować problemy, gdy ktoś jeszcze nie zaktualizował kluczy na serwerze)   

```
.         <-- DS (klucz prywatny)
pl.       <-- DS
piwo.pl.  
```

`piwo.pl.` autoryzuje się w serwerze poziom wyżej - więc musi przekazać mu swój klucz???  


problem z DNSSEC: NSEC - jeżeli zarejestrujemy sobie domenę `piwo.pl` i zrobimy poddomenę `dobre.piwo.pl` - to odpowiedź musi być podpisywana    
natomiast jeżeli jakiejś domeny nie ma, to potencjalnie ktoś mógłby się pod nią podszyć - więc odpowiedź na brak domeny też musi być podpisana  
problem ten został rozwiązany przez NSEC3, NSEC5  

______

## Sieci bezprzewodowe (wifi) i sieci przewodowe
> podobno coś o wifi było  na pierwszym wykładzie

- w sieciach przewodowych gdy 2 komputery nadają na raz to jest zwiększona amplituda (powstaje przepięcie) i wykrywamy kolizję
- w sieciach bezprzewodowych nie jesteśmy w staanie wykryć takiego przepięcia, ale możemy wykryć gdy ktoś nadaje więc będziemy starali się uniknąć kolizji



