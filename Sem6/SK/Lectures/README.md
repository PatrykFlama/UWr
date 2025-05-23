[TOC]

# Notatki
## Warstwowe modele
### Z szyfrowaniem
![image](image.png)


# Zagadnienia
## Wykład 1
- Co to jest protokół komunikacyjny? Dlaczego wprowadza się warsty protokołów?  
- Wymień warstwy internetowego modelu warstwowego. Jakie są zadania każdej z nich?  
- Jakie warstwy są zaimplementowane na komputerach a jakie na routerach?  
- Czym różni się model warstwowy TCP/IP od OSI?  
- Co jest potrzebne do zbudowania dwukierunkowego niezawodnego kanału?  
- Porównaj wady i zalety przełączania obwodów i przełączania pakietów.  
- Jakie znasz rodzaje multipleksowania? Po co i kiedy się je stosuje?  
- Porównaj ze sobą rodzaje routingu.  
- Co to jest komunikacja pełnodupleksowa, półdupleksowa i simpleksowa?  
- Do czego służy polecenie traceroute? Co pokazuje?  
- Po co stosuje się bufory w routerach? Co to jest przeciążenie?  
- Jakie są przyczyny opóźnień pakietów?  
- Co to jest BDP? Co to jest czas propagacji?  
- Wyjaśnij pojęcia: komunikacja simpleksowa, półdupleksowa, pełnodupleksowa.  
- Co umożliwia protokół IP? Co to znaczy, że protokół realizuje zasadę best effort?  
- Jakie są zalety i wady zasady end-to-end?  
- Po co wprowadza się porty?  
- Wyjaśnij pojęcie enkapsulacji i dekapsulacji.  

## Wykład 2
- Z czego wynika hierarchia adresów IP? Jaki ma wpływ na konstrukcję tablic routingu?  
- Notacja CIDR.  
- Co to jest adres rozgłoszeniowy?  
- Co to jest maska podsieci  
- Opisz sieci IP klasy A, B i C.  
- Co to jest pętla lokalna (loopback)?  
- Do czego służy pole TTL w pakiecie IP? Do czego służy pole protokół?  
- Jakie reguły zawierają tablice routingu?  
- Na czym polega reguła najdłuższego pasującego preﬁksu?  
- Co to jest trasa domyślna?  
- Do czego służy protokół ICMP? Jakie znasz typy komunikatów ICMP?  
- Jak działa polecenie ping?  
- Jak działa polecenie traceroute?  
- Dlaczego do tworzenia gniazd surowych wymagane są uprawnienia administratora?  
- Co to jest sieciowa kolejność bajtów?  
- Co robią funkcje socket(), recvfrom() i sendto()?  
- Jakie informacje zawiera struktura adresowa sockaddr_in?  
- Co to jest tryb blokujący i nieblokujący? Co to jest aktywne czekanie?  
- Jakie jest działanie funkcji select()?  

## Wykład 3
- Co to jest cykl w routingu? Co go powoduje?  
- Czym różni się tablica routingu od tablicy przekazywania?  
- Dlaczego w algorytmach routingu dynamicznego obliczamy najkrótsze ścieżki?  
- Co to jest metryka? Jakie metryki mają sens?  
- Czym różnią się algorytmy wektora odległości od algorytmów stanów łączy?  
- Jak router może stwierdzić, że bezpośrednio podłączona sieć jest nieosiągalna?  
- Co to znaczy, że stan tablic routingu jest stabilny?  
- Jak zalewać sieć informacją? Co to są komunikaty LSA?  
- Co wchodzi w skład wektora odległości?  
- W jaki sposób podczas działania algorytmu routingu dynamicznego może powstać cykl w routingu?  
- Co to jest problem zliczania do nieskończoności? Kiedy występuje?  
- Na czym polega technika zatruwania ścieżki zwrotnej (poison reverse)?  
- Po co w algorytmach wektora odległości deﬁniuje się największą odległość w sieci (16 w protokole RIPv1)?  
- Po co stosuje się przyspieszone uaktualnienia?  
- Co to jest system autonomiczny (AS)? Jakie znasz typy AS?  
- Czym różnią się połączenia dostawca-klient pomiędzy systemami autonomicznymi od łącz partnerskich (peering)?  
- Dlaczego w routingu pomiędzy systemami autonomicznymi nie stosuje się najkrótszych ścieżek?  
- Które trasy w BGP warto rozgłaszać i komu? A które wybierać?  
- Jak BGP może współpracować z algorytmami routingu wewnątrz AS?  


## Wykład 4
- Co to są prywatne adresy IP? Jakie pule adresów są zarezerwowane na takie adresy?  
- Co robi funkcja bind()?  
- Czym różnią się porty o numerach mniejszych niż 1024 od innych?  
- Jakie są zadania procesora routingu, portu wejściowego, portu wyjściowego i struktury przełączającej?  
- Czym się różni przełączanie pakietów w routerze za pomocą RAM od przełączania za pomocą struktury przełączającej?  
- Jakie są pożądane cechy struktury przełączającej w routerze?  
- Gdzie w routerze stosuje się buforowanie? Po co?  
- Po co w portach wyjściowych klasyﬁkuje się pakiety?  
- Co to jest blokowanie początku kolejki? Gdzie występuje? Jak się go rozwiązuje?  
- Rozwiń skrót LPM.  
- Jakie znasz struktury danych implementujące LPM? Porównaj je.  
- Co to jest pamięć TCAM? Jak można ją zastosować do implementacji LPM?  
- Na czym polega fragmentacja IP? Gdzie się ją stosuje i dlaczego? Gdzie łączy się fragmenty?  
- Co to jest MTU? Na czym polega technika wykrywania wartości MTU dla ścieżki?  
- Jak działa szeregowanie pakietów w buforze wyjściowym routera?  
- Jakie są różnice pomiędzy nagłówkami IPv4 i IPv6?  
- Zapisz adres IPv6 0321:0000:0000:0123:0000:0000:0000:0001 w najkrótszej możliwej postaci.  
- Co to jest tunelowanie 6in4?  
- Na czym polega NAT i po co się go stosuje? Jakie są jego zalety i wady?  
- Jaki stan musi przechowywać router z funkcją NAT?  

## Wykład 5
- Jakie są zadania warstwy łącza danych a jakie warstwy ﬁzycznej?  
- Czym różni się koncentrator od przełącznika sieciowego?  
- Jak działa algorytm rundowy i bezrundowy ALOHA?  
- Jak działa algorytm odczekiwania wykładniczego?  
- Wyjaśnij skróty CSMA/CD i CSMA/CA.  
- Opisz budowę ramki Ethernetowej.  
- Co to jest adres MAC?  
- Do czego służy tryb nasłuchu (promiscuous mode)?  
- Po co w Ethernecie deﬁniuje się minimalną długość ramki?  
- Do czego służą protokoły ARP i DHCP?  
- Czym różni się łączenie dwóch sieci za pomocą mostu od łączenia ich za pomocą routera?  
- Jak warstwa łącza danych realizuje rozgłaszanie?  
- Na czym polega tryb uczenia się w przełączniku sieciowym?  
- Po co w przełączanym Ethernecie stosuje się algorytm drzewa spinającego?  
- Co to jest sieć VLAN? Po co się ją stosuje?  
- Wyjaśnij zjawisko ukrytej stacji.  
- Na czym polega rezerwowanie łącza za pomocą RTS i CTS?  

## Wykład 6
- Co może stać się z przesyłanym ciągiem pakietów IP podczas zawodnego i niezawodnego transportu?  
- Co to jest kontrola przepływu?  
- Czym różnią się protokoły UDP i TCP? Podaj zastosowania każdego z nich.  
- Co to jest segmentacja? Dlaczego segmenty mają ograniczoną wielkość? Rozwiń skrót MSS.  
- Jak nazywają się jednostki danych przesyłane w kolejnych warstwach?  
- Jak małe pakiety zmniejszają opóźnienie przesyłania danych?  
- Wytłumacz znaczenie skrótów RTT i RTO. Na jakiej podstawie ustalana jest wartość RTO?  
- Jak protokoły niezawodnego transportu wykrywają duplikaty pakietów i potwierdzeń?  
- Opisz algorytm Stop-and-Wait. Jakie są jego wady i zalety?  
- Do czego służą numery sekwencyjne w niezawodnym protokole transportowym?  
- Opisz algorytm okna przesuwnego.  
- Jaki jest związek między rozmiarem okna a BDP (bandwidth-delay product)?  
- Opisz i porównaj następujące mechanizmy potwierdzania: Go-Back-N, potwierdzanie selektywne, potwierdzanie   skumulowane.
- Dlaczego istotne jest potwierdzanie odbioru duplikatów segmentów?  
- Co to jest okno oferowane? Jak pomaga w kontroli przepływu?  
- Jakie mechanizmy niezawodnego transportu i kontroli przepływu implementowane są w protokole TCP?  
- Na czym polega opóźnione wysyłanie ACK w protokole TCP?  
- Na czym polega mechanizm Nagle'a? Kiedy nie należy go stosować?  
- Co oznaczają pola ,,numer sekwencyjny'' i ,,numer potwierdzenia'' w nagłówku TCP?  
- Czy warstwa transportowa implementowana jest na routerach? Dlaczego?  
- Sformułuj słabą i silną zasadę end-to-end.  

## Wykład 7
- Co to jest gniazdo?  
- Czym różni się gniazdo nasłuchujące od gniazda połączonego? Czy w protokole UDP mamy gniazda  
połączone?
- Co robią funkcję jądra bind(), listen(), accept(), connect()?  
- Czym różni się komunikacja bezpołączeniowa od połączeniowej?  
- Czym różni się otwarcie bierne od otwarcia aktywnego? Czy serwer może wykonać otwarcie aktywne?  
- Do czego służą ﬂagi SYN, ACK, FIN i RST stosowane w protokole TCP?  
- Opisz trójstopniowe nawiązywanie połączenia w TCP. Jakie informacje są przesyłane w trakcie takiego  
połączenia?
- Dlaczego przesyłanych bajtów nie numeruje się od zera?  
- Jakie segmenty są wymieniane podczas zamykania połączenia w protokole TCP?  
- Co zwraca funkcja recv() wywołana na gnieździe w blokującym i nieblokującym trybie?  
- Po co wprowadzono stan TIME_WAIT?  
- Na podstawie diagramu stanów TCP opisz możliwe scenariusze nawiązywania i kończenia połączenia.  

## Wykład 8
- Opisz budowę adresu URL. Opisz budowę adresu URL w przypadku schematu http.  
- W jakim celu serwer WWW ustawia typ MIME dla wysyłanej zawartości? Podaj kilka przykładów typów MIME.  
- Po co w nagłówku żądania HTTP/1.1 podaje się pole Host?  
- Do czego służą pola Accept, Accept-Language, User-Agent, Server, Content-Length, Content-  
Type w nagłówku HTTP?
- Jak implementuje się przechowywanie stanu w komunikacji HTTP?  
- Jak wygląda warunkowe zapytanie GET protokołu HTTP?  
- Jakie znasz kody odpowiedzi protokołu HTTP?  
- Na czym polegają połączenia trwałe w HTTP/1.1? Do czego służy opcja Connection: close w nagłówku HTTP?  
- Po co stosuje się metodę POST?  
- Co to jest technologia REST?  
- Do czego służą serwery proxy?  
- Co to jest odwrotne proxy? Co to jest CDN?  
- Jak skłonić klienta, żeby łączył się z serwerem proxy a nie bezpośrednio ze stroną WWW?  
- Jakie informacje dołączane są przez serwer proxy do zapytania?  
- Co to są anonimowe serwery proxy?  
- W jakim celu powstał protokół QUIC? Jakie funkcje spełnia?  


## Wykład 9
- Jaki jest cel systemu nazw DNS?  
- Do czego służy plik /etc/hosts?  
- Rozwiń skrót TLD (kontekst: DNS), podaj parę przykładów.  
- Czym są strefy i delegacje DNS?  
- Czym różni się rekurencyjne odpytywanie serwerów DNS od iteracyjnego?  
- Jak działa odwrotny DNS? Jaki typ rekordów i jaką domenę wykorzystuje?  
- Jakie znasz typy rekordów DNS? Co to jest rekord CNAME?  
- Do czego służy protokół SMTP a do czego IMAP?  
- Co to są przekaźniki SMTP (relays)?  
- Jaki rekord DNS jest sprawdzany przed wysłaniem poczty do danej domeny?  
- Wymień parę popularnych pól w nagłówku maila. Do czego służą pola Received i Bcc?  
- Co umożliwia standard MIME?  
- Co to jest spam? Jakie znasz metody walki ze spamem?  
- Na czym polega mechanizm SPF?  
- Jaka jest rola trackera w sieci Bittorrent?  
- Po co w plikach .torrent stosuje się funkcje skrótu?  
- Jakie są różnice w postępowaniu seedera i leechera w sieci BitTorrent?  
- Na czym polegają połączenia odwrócone? Jak stosuje się je w protokole FTP?  
- Opisz podobieństwa i różnice asymetrycznych (cone) NAT (pełnego i ograniczonego) i symetrycznych NAT.  
- Opisz technikę wybijania dziur (hole punching) w NAT. Po co konieczny jest serwer pośredniczący?  


## Wykład 10
- Jakie znasz typy kodów detekcyjnych? Do czego służą i jakie są między nimi różnice?  
- Jakie rodzaje błędów mają wykrywać kody detekcyjne? Z czego biorą się błędy przy przesyłaniu  
danych?
- Jak działa algorytm obliczania sum kontrolnych CRC?  
- W jaki sposób działa wykrywanie błędów przy sumie kontrolnej CRC?  
- Jakie znasz metody korygowania błędów w transmisji?  
- Co to jest (a,b)-kod? Podaj przykład.  
- Co to jest odległość Hamminga? Jak wpływa na możliwość detekcji i korekcji błędów?  
- Do czego służą kody MAC? Co to jest HMAC?  
- Jakie własności powinna mieć kryptograﬁczna funkcja skrótu?  
- Czym różni się poufność od integralności?  
- Co to są szyfry monoalfabetyczne? Dlaczego łatwo je złamać?  
- Na czym polegają ataki z wybranym tekstem jawnym, znanym tekstem jawnym i znanym  
szyfrogramem?
- Co to jest szyfrowanie one-time pad?  
- Na czym polega szyfrowanie blokowe? Czym różni się tryb ECB od CBC?  

## Wykład 11
- Czym szyfrowanie symetryczne różni się od asymetrycznego?  
w szyfrowaniu symetrycznym obie strony muszą znać ten sam klucz, mogą zarówno szyforwać jak i deszyforować wiadomości  
w szyfrowaniu asymetrycznym do zaszyfrowania i odszyfrowania wiadomości używa się dwóch różnych kluczy (publiczny i prywatny), więc np tylko jedna strona może odczytać zaszyfrowaną wiadomość  

- Na czym polega bezpieczeństwo przy szyfrowaniu asymetrycznym?  
klucz deszyfrujący może zostać u odbiorcy - nie ma problemu z tym jak go przekazać, a klucz szyfrujący może być publiczny tak aby każdy mógł zaszyfrować wiadomość do odbiorcy

- Opisz algorytm RSA.  
- Czy różni się szyfrowanie od uwierzytelniania?  
szyfrowanie zapewnia poufność wiadomości, a uwierzytelnianie zapewnia tożsamość nadawcy  

- Co to jest atak powtórzeniowy?  
- Czy w szyfrowaniu asymetrycznym szyfrujemy kluczem publicznym czy prywatnym?  
szyfrujemy kluczem publicznym, a odszyfrowujemy kluczem prywatnym

- Na czym polega podpisywanie wiadomości? Jakim kluczem to robimy?
podpisujemy wiadomość kluczem prywatnym, aby każdy mógł ją zweryfikować kluczem publicznym kto ją podpisał (czyli ma dostęp do klucza prywatnego) - dowodzi to że nadawca wiadomości ma poprawny klucz prywatny do klucza publicznego

- Jak można wykorzystać podpisy cyfrowe do uwierzytelniania?  
- Czy HMAC można wykorzystać do uwierzytelniania? Czy HMAC jest podpisem cyfrowym?  
- Dlaczego lepiej podpisywać funkcję skrótu wiadomości niż samą wiadomość? Z jakim ryzykiem się to wiąże?  
gdybyśmy podpisywali wiadomość to nasz podpis byłby tak samo długi jak wiadomość, a po zastosowaniu funkcji skrótu mamy krótszą wiadomość do podpisania (więc nie marnujemy przepustowości łącza)  
wiąże się to z ryzykiem kolizji - funkcja skrótu jest znana każdemu więc można np przeprowadzić atak urodzinowy lub

- Co to są certyﬁkaty? Co to jest ścieżka certyﬁkacji?  
- Co to jest urząd certyﬁkacji (CA)?  
- Jak TLS zapewnia bezpieczeństwo połączenia?  
- W jaki sposób w TLS następuje uwierzytelnienie serwera, z którym się łączymy?  
- Co to są klucze sesji? Po co się je stosuje?  
- Co to są kolizje kryptograﬁcznej funkcji skrótu?  
- Na czym polega atak urodzinowy?  
znalezienie wiadomości m' dla danej wiadomości m, o takim samym skrócie jest 2-krotnie cięższe niż znalezienie takich dwóch wiadomości m i m', które mają ten sam skrót (atakujący więc może wtedy łatwo poprosić o podpisanie wiadomości m, a wysłać wiadomość m')

