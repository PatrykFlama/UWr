# lista 7
(firewall/iptables/nftables)

Można używać dowolnego systemu z iptables LUB nftables (zapewne każdy linux, PrASKowa VMka lub PrASKowy router). Konfiguracja iptables na OpenWrt jest dość mocno zakręcona ;).

Jeśli ktoś ma ochotę jednak robić na OpenWrt - nie widzę przeszkód (w razie czego - ostrzegałem). Listę można w takim wypadku też (próbować) robić za pomocą pliku konfiguracyjnego /etc/config/firewall.

Rozwiązań oczekuję w formie listy poleceń (iptables -foo -bar ..) lub nftables.conf, które się wykonują BEZ BŁĘDÓW.

Jeżeli ktoś życzy sobie robić listę na firewallu sprzętowym dowolnego producenta, na BSDowym (i)pf, czy też nawet na Windowsie - nie widzię przeszkód. Cel zadań ma być osiągnięty.

Testowanie oraz metodyka wykonywania zadań:

- (zalecana konfiguracja) iptables/nftables na VM, sieć wewnętrzna - sieć w Twoim VPN/sieci dla VM/kontenerów (np. 10.0.0.0/24)
- skrypty można pisać “na sucho” w dowolnym edytorze lub próbować bezpośredio w konsoli
- do VMek można się dostać inaczej niż przez ssh (⇒ konsola Proxmox) w razie “odcięcia się”, co zdarza się każdemu, szczególnie na początku przygód z firewallem :)


Uwagi:

- Trzeba włączyć przekazywanie pakietów w linuksie (echo 1 > /proc/sys/net/ipv4/ip_forward)
- Regułki można pisać w notatniku, skrypcie, itp.
- pomoce: internety, google, kaczkakaczkaidź.pl, man iptables, man iptables-extensions


Zadania:

1. Za pomocą nftables/iptables* (zamień “nftables/iptables” na dowolnie wybrany firewall) udostępnić sieć komputerom w podsieci vm1 i vm2 podłączonych do jednego z interfejsów sieciowych (ens19), używając translacji adresów na adres zewnętrzny (chodzi o NAT źródłowy).
Co byś zrobił(a) gdyby adres zewnętrzny był zmienny (podpowiedź: SNAT vs MASQUERADE)? (1p)
2. Używając nftables/iptables skonfiguruj firewall typu stateful, który przepuszcza:
   - połączenia nawiązane oraz będące odpowiedzią na ruch pochodzący z wewnątrz (stateful firewall!)
   - połączenia do OpenVPN (domyślnie port 1194 UDP)
   - połaczenia na port 22 (TCP) z dowolnego adresu
   - połączenia na port 53 (UDP i TCP) z dowolnego adresu (DNS dla VMek!)
   - połączenia na port 80 i 443 (TCP) z dowolnego adresu (jako jedna regułka!)
   - żądania icmp echo ale nie więcej niż jedno na sekundę
   - zakres portów od 2000:3000 (TCP)
   - połączenia na port 113 (TCP) mają dostawać natychmiastową odpowiedź icmp-port-unreachable.
    Pozostałe porty mają całkowicie odrzucać połączenia (DROP). (2p)
3. W tak (j/w) skonfigurowanej sieci przekieruj port zewnętrzny 222 (TCP) na wybrany komputer sieci wewnętrznej (np. vm1), port 22 (TCP). (1p)
4. Za pomocą nftables/iptables spowoduj by wpisanie w przeglądarce na komputerze wewnętrznym adresu http://www.google.com powodowało wyświetlenie strony http://www.wolframalpha.com (ignorujemy błedy SSL). (1p)
5. Przekieruj wszystkie zapytania DNS z sieci wewnętrznej na inny (swój) serwer DNS. (1p)
6. (ping fun) Napisz skrypt lub program, który co sekundę zmienia odpowiedź serwera na żądania icmp echo (jest conajmniej 7 różnych odpowiedzi na icmp echo request). (2p)
7. Jeden z użytkowników Twojego serwera (posiada konto shellowe (ale nie znajduje się w sieci lokalnej!)) jest podejrzany o sterowanie botnetem zarządzanym z IRC-a. Zaloguj wszystkie pakiety przesyłane do serwerów IRC pochodzące od tego użytkownika. (1p)
To nie jest wymyślony scenariusz. Poczytaj np. tu: https://www.forensicfocus.com/articles/dissecting-malicious-network-traffic-to-identify-botnet-communication/
8. W Twojej sieci wewnętrznej jeden z komputerów „nie ma internetu”. Odkryłeś(aś), że Twój operator blokuje pakiety z TTL < 64. Napraw to za pomocą nftables/iptables. (1p)
9. Za pomocą nftables/iptables zabroń komputerowi o adresie MAC 00:ca:fe:ba:be:00 możliwość przesyłania pakietów na port 80 mniejszych niż 50 i większych niż 1000 bajtów, ale tylko w piątki jeśli jest 13 dzień miesiąca. (1p)
10. Za pomocą programu ip z pakietu iproute2 (linux, zainstalowane domyślnie) dodaj dodatkowy dowolnie wymyślony adres IP (tzw. alias) do wybranego interfejsu. (1p)
11. Za pomocą programu ip ustaw statyczną trasę do sieci 1.1.1.0/24 przez router 1.1.1.1. (Uwaga! Polecenie ma się wykonać! Co trzeba zrobić, żeby to zadziałało?) (1p)
12. Do konfiguracji z zadania 1 dodaj założenie, że sieci lokalnych podłączonych do routera jest więcej niż jedna. Dodatkowo są to sieci 11.12.13.0/24 i 18.19.20.0/23. Zmodyfikuj swoje reguły nftables/iptables tak, aby ruch między tymi sieciami (z sieci A do B) nie był NATowany, ale z tych A i B do internetu już miał NAT. (2p)
13. zadania (raczej pomijając te związane z NATem) wykonaj również dla ruchu IPv6 (1p)
14. Posiadasz łącze w technologii (x)dsl. W Twojej sieci wewnętrznej komputery mają problemy z internetem. Objawy: nie działają niektóre strony. Wszystkie pingi działają prawidłowo.
Na samym routerze wszystko działa prawidłowo. Napraw to za pomocą nftables/iptables.
Dodatkowe pytania na temat zadania można zadawać na wykładzie, pracowni, stronie konsultacji lub mailem. (4p)
