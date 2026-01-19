# lista 4 / list no. 4
1. Zainstaluj OpenVPN na swojej wirtualce (1p)
2. Skonfiguruj OpenVPN na wirtualce jako serwer. (1p)
    1. Konfiguracja z autoryzacją po kluczach SSL. (możesz użyć CA z poprzedniej listy albo zrobić nowe - dowolnie) (2p)
    2. Konfiguracja z autoryzacją po nazwie użytkowniaka z systemu (PAM) (2p)
3. Skonfiguruj OpenVPN na laptopie (lub routerze PrASKowycm) jako klient dla VPN na wirtualce.
Klient - router/laptop powinien łączyć się do serwera VPN i wpuszczać po w tunelu ssh zaraz po zbootowaniu oraz niezależnie od łącza. (2p)
4. Skonfiguruj WireGuard na wirtualce jako serwer a na routerze/laptopie/wirtualce) klienta; zestaw tunel VPN za pomocą WireGuard analogicznie do poprzedniego zadania. (2p)
5. Po połączeniu przez VPN na Twoją VM uzyskaj 10/10 na stronie http://test-ipv6.com/ - trzeba dodać obsługę IPv6 do OpenVPN/WireGuard. (4p)
wersja dla leniwych: dowolne połączenie ipv6 (udowodnione na pracowni), które daje 10/10 i NIE jest tunelowaniem “teredo” jest warte 2p (to jest alternatywa dla tego zadania, a nie dodatkowe 2p).
6. Skonfiguruj usługę tunelu TCP over DNS np. https://github.com/yarrick/iodine | pamiętaj, że Twój serwer DNS i usługa IoD MUSZĄ działać razem. (3p)
