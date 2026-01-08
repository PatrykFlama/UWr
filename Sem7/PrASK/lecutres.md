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


# Lecture 3 - x.500
bonding (link bond) - możemy sobie skonfigurować  w linuksie taki agregator linków  
> np możliwe jest już łączenie wifi+ethernet  

> ile razy można spojrzeć w światłowód?  
> 2 razy - raz lewym a raz prawym okiem

LDAP - lekka wersja takiej usługi, np do przechowywania adresów ip, DNS, etc

x.509 - ssl, protokół zabezpieczenia usług katalogowych

HSTS - skoro mamy już certyfikat, a istanieją ataki SSL striking (istnieje już bezpieczne połączenie ssl, a ktoś nam mówi 'jednak idź do portu bez ssl') - więc będziemy wymuszać aby korztsać z SSL do połączenia z naszą stroną. Robi się to za pomocą specjalnego nagłówka (zawsze przychodź do mnie po ssl) oraz robienie przekierowania z http na https
> gdy HSTS jeszcze raczkował, to można było wpisać mu (publicznie) swoje storny które miały mieć wymuszone ssl - były one wkompilowane w kod przeglądarki

dodatkowo HSTS można konfigurować na TLD - więc np google dodal do stron .zip HSTS  

___

kiedyś http działało sobie zwykłym tekstem (idea: komunikacja między serwerema ma być human-readable)  
jest to jednak niewydajne (lepiej przesyłać strumień danych)

___

co jak nie ufamy naszym urzędom, ISP? jak szyfrować zapytania DNS?
- DNS over TLS, mniej popularne
- DOH (DNS over HTTPS)


analogiczne pytanie można zadać odnośnie urzędów certyfikacji - dalczego mamy im ufać? powstała więc odnoga bazująca na DNS - DANE (DNS-based Authentication of Named Entities).
jest to decentralizowany system (niespecjalnie popularny), gł wykorzystywany w mailach (gdzie rzadko sprawdzamy certyfikaty)  

DNSviz - strona do sprawdzania DNSSec
___

fajna strona hardenize - dobre praktyki konfiguracyjne dla serwerów publicznych

http3check - testowanie wsparcia dla http3



___


(ram) ballooning - pamięć ram może być współdzielona, wtedy możemy teoretycznie zadeklarować procesom/writualnym maszynom więcej pamięci, niż faktycznie mamy  

___

jak stworzyć kom[puter w sieci 10.0.0.1/32 ?  

załóżmy że mamy komputer w 10.0.0.2/30 oraz gateway w 10.0.0.1/30 - wtedy mamy:
- 10.0.0.0/30 przypisane do interfejsu enp0
- w tablicy routingu wpisane 0.0.0.0 (default) -> 10.0.0.1

teraz jeżeli chcemy mieć gateway na 1.2.3.4 to komputer nie wie gdzie to jest, dodajemy więc do naszego interfejsu ten adres (przpisujemy go) - wtedy nasz komputer nadal wie jak się dostać do routera, mimo tego że jest on poza siecią:
- 10.0.0.2/32 enp0
- 1.2.3.4 enp0
- default via 1.2.3.4

> - `ip addr` - lokalne przypisania
> - `ip route` - sprawdza tablicę routingu

___

> arp proxy - na wszystkie zapytania arp odpowiada router


## NAT i IPv6
przyjmujemy że najmniejsza 'fajna' podsieć ipv6 to /64 (przy mniejszych rzomiarach nie działają niektóre funkcje, np reklamowanie routera)
> zdarzają się przypadki gdy ISP da nam np tylko jeden adres ipv6 - wtedy nat na ipv6 ma sens

przyjmijmy że mamy 2001::a/64 przypisane do maszyny na enp18  
niech router będzie na 2001::1 (komunikuje się z naszym komputerem protokołem NDP)  
jeżeli stworzymy tune wg0 z adresem 2001:1::cafe/128 to nie będzie on w stanie się komunikować z routerem  
musimy więc zrobić coś takiego jak NDP proxy: `ip -6 neigh proxy add WG_ADDR dev enp18` - żeby zachowało się to po reboocie, to musimy w configu vpn'a dodać  

> `ip neigh sh` - pokazuje tablicę przypisać mac <-> iface



# kontenery
- `chroot`
- `namespaces`

to jest zalążek do konceptu kontenera - kontener to proces który myśli że jest initem (system init)  
> np nginxa z chrootem w `/var/www` oraz pid 9191 -> 1 

supervisor to będzie proces dbający o to że jeżeli taki nginx nam padnie, to zostanie uruchomiony ponownie


docker sam w sobie jest już supervisorem

idea: każdy kontener powinien zawierać tylko jeden proces - jeżeli zabijemy ten proces (pid 1) to ubijamy całe drzewo procesów    


> zaleta konteneryzacji - lżejsza od wirtualizacji bo jest lżejsze, działamy na tym samym kernelu etc

problem z dockerem - działa on z uprawnieniami roota;
podman, lxc - tutaj juz ten problem został rozwiązany


w dockerze przyjęło się dzielenie środowiska na obrazy oraz kontenery  
obrazy to template wg którego tworzone są kontenery, z konceptu obrazu nie można ruszać/zmienić  


z założenia kontenery powinny być efemeryczne - zabicie go i uruchomienie ponownie nie powinno 'zepsuć jest stanu'  

___

koncept dodatkowej redundancji przy 2 kontenerach nginx: stawiamy jeden 'zapasowy', sprawdzają się one nawzajem - czy ten drugi żyje, jeżeli master przestał odpowiadać to slave przejmuje jego ip oraz działanie (pokazuje na nie inny nginx, reverse proxy https)


# IaC and stuff
## Ansible
automation / configuration management  

## IPXE
bootowanie z interfejsu sieciowego

ipxe.org

> 'make linux fast again' - strona wyłączająca w kernelu moduły bezpieczeństwa, ale przyspieszająca (zauważalnie 30%-40%) linuxa  
> pakiet z mikrokodem np patchuje rzeczy w środowisku kernela, aby dalej był bezpieczny (ale wolniejszy)

## Backupy





# Firewalle
firewalle działają na warstwach 2-7 (OSI)

> w zasadzie na warstwie 1 też mogą działać - odłączanie kabla  

> firewall na warstwie 8 to fizyczna kontrola dostępu do serwerowni - ha ha ha


czym się różni firewall od NAT? nie zmienia pakietów  

firewalle dzielimy na stanowe (np trackowanie TCP - ruch established i related) i bezstanowe (bardzo ograniczone zastosowanie)

np stanowy firewall może zapisać naszą sesję (pary ip-port źródłowy-docelowy)
- related, gdy otwieramy sesję i dostajemy odpowiedź zwrotną
- established, gdy mamy komunikację z ustanowionej sesji

gdy odrzucamy pakiet (zamiast drop robimy reject) to głównie wysyłamy informację zwrotną korzystając z flagi rst (lub dla udp korzystamy z ICMP)


firewall pakietów w kernelu - ich ścieżki rozróżniamy na: input, output i forward
- jeżeli system wspiera forward, to z definicji jest routerem (i wtedy rzadko kiedy przyjmuje/generuje pakiety)
- zazwyczaj host korzysta z input (output w standardowych konfiguracjach jest pomijany)

![netfilter packet flow](https://upload.wikimedia.org/wikipedia/commons/thumb/3/37/Netfilter-packet-flow.svg/2560px-Netfilter-packet-flow.svg.png)


## WAF
są rzeczy które możemy takim klasycznym firewallem filtrować (np geolokalizacja, max 100 zapytań na sekundę, etc), ale są też takie jak np XSS - z którymi firewall do warstwy 6 sobie nie poradzi  
ale firewalle działające na warstwie 7 potrafią zdekodować http - rozumieją co się dzieje w środku zapytania - 'deep packet inspection'  

np jeżeli w zapytaniu siedzie 'DROP TABLE USERS' to możemy łatwo wykryć że coś jest nie tak, tkzw WAF (Web Application Firewall)

> działa to podobnie jak antywirus - taki firewall patrzy na feedy, szuka patternów i blokuje jeżeli coś mu nie pasuje


jak działa cloudflare? jak mamy serwer web'owy - to jest on narażony na czynniki zewnętrzny (powinien implemenować firewalle, WAF, zabezpieczenie przed DOS)  
więc zamiast tego możemy się schować za cloudflare'em (blokujemy cały ruch zewnętrzny bezpośrednio do naszego ip), łączymy się np vpn do cloudflare'a - i tylko on ma prawo z nami rozmiaiać  
cały ruch zewnętrzny idzie do cloudflare'a (który ma porządne warstwy zabezpieczeń), więc cały ruch który do nas idzie powinien być bezpieczny

> cloudflare jest 'za' popularny - efekty awarii samego cloudflare'a


## IDS, IPS - Intrusion Detection System, Intrusion Prevention System
powiedzieliśmy sobie że firewall patrzy na pakiet i decyduje czy go przepuścić  

np jeżeli z jakiegoś adresu IP dostaliśmy skan na wszystkie porty, a potem otrzymujemy od niego połączenie ssh - brzmi podejrzanie  

**tutaj chcemy patrzeć na korelację**  


- IDS - sprawdzamy i informujemy  
- IPS - sprawdzamy i blokujemy


## UTM
UTM - koreluje rzeczy powiązane z firewallem, głęboką inspekcją pakietów, etc, z listą CVE

> lista CVE - lista podatności

jeżeli zaobserwuje że coś próbuje wykorzystać daną podatność, to blokuje ten atak



## DMZ
```
WAN <--> <nasz router> <--> LAN
            /\
            ||
            ||
            \/
           DMZ
```


strefa zdemilitaryzowana - znajduje się w niej nasz serwer, jest to podsieć która z założenia jest dostępna z zewnątrz  
potrzebujemy do tego reguły firewalla, które odpowiednio blokują ruch aby utrzymać konfigurację bezpieczną (jest to sporo regułek)   


zone based firewall - więc powstał koncept, gdzie dzielimy sobie naszą sieć na strefy (np internal, external, dmz) - reguły definiujemy strefami (a nie interfejsami), strefy mają przyisane liczby i ruch może iść tylko z liczby mniejszej do większej (+ jakieś wyjątki np ze strefy 3 (WAN) może iść ruch do LAN (1))  

można to też wyciągnąć na poziom wyższy, gdzie zamiast stref myślimy o obiektach (strefa, interfejs, aplikacja, mac adres)  

## ZTNA - zero trust network access
wszystkich klientów traktujemy jako tych złych - z założenia każdy jest w wan i nie może ze sobą gadać

wprowadzamy więc NAC - Network Access Control - który ustanawia połączenia z innymi urządzeniami w formie tunelów, tylko konkretne uprawnione połączenia są tworzone/zezwalane




# Firewalle
## Podsumowanie
- iptables powoli są deprecowane
- nftalbes generalnie są lepsze, mają wiele braków iptables naprawione - ale iptables są bardziej dojarzałym (i kompletnym) produktem

> jednego z zadań na liście ćwiczeniowej 3 lata temu nie dało się zrobić, ale podobno zostało to już poprawione

- w iptables utrzymywało się 2 osobne firewalle dla IPv4 i IPv6 (w nftables jest to lepiej rozwiązane)
- setup ipset'u był ciężki w iptables, w nftables jest on wbudowany (ipset 'opis' tego czym jest pewien zbiór adresów)


## NAT
nat harping (nat pining) - gdy ktoś z WAN próbuje się przez NAT dostać 'z powrotem' do naszej sieci

nat helpers, szczególnie przydatne do FTP - np w odpowiedzi protokołu, która powie na jakiem porcie odbędzie się przesyłanie plików, przejrzy ją i otworzy odpowieni port  
> to nie to samo co UPnP - mamy usługę oczekującą że wszyscy z internetu będą w stanie się do nas dostać  
> wysyłamy więc taką informację do routera, żeby otworzył on dany port (co wsm jest niebezpieczne - jak tylko ktoś się do nas dostanie to sobie otworzy wszystkie porty)


## gdzie dać firewalle
wyobraźmy sobie strukturę serwerów o maksymalnej redundancji sieciowej ([2 routery] --mesh--> [switche] --mesh--> [maszyny front] --mesh--> [maszyny back]  --mesh--> [bazy danych])  
gdzie powinien być firewall? wszędzie. (jest to dyrektywa PCI-DSS)  
dodatkowo w takiej konfiguracji warto mieć połączenia mTLS aby uwierzytelniać połączenia (żeby nikt będący jużw sieci się nie mógł podszyć pod maszynę)


## Logi
tworzymy osobną encję (architekturą może to byc maszyna, albo coś podobnego do wyżej) _loghost_ do której wszystkie maszyny będą logować  

> shell shock - każdy użytkownik z dostępem do basha mógł ustawić zmienną środowiskową, która czasem była wywoływana z uprawnieniami roota  

w sony, korzstając z log4j, podatność shell shock została wykorzystana aby postawić VPN na loghoście - który nie był zabezpieczony (z konceptu odcięty od świata zewnętrzenego)  
wniosek - wszędzie zakładamy firewalla, warto też zabezpieczać output (nie tylko input)

np dla loghosta w ogóle chcielibyśmy ograniczyć dostęp do internetu - możemy to zrobić za pomocą zwykłego forward proxy


- ELK stack - log stash
- Elastic search - baza danych
- Kibana - prezentowanie danych














