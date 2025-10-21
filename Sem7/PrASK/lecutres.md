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

