# lista 5 / list no. 5
Dla chętnych na zabawę sprzętem: https://ii.czk.mk/routery

1. Zainstaluj serwer FreeRadius na swoim routerze lub vm (można używać własnych routerów w trybie klienta RADIUS lub klienta i serwera, lub tylko VM (PrASkowe, LUB na swoim komputerze) jako serwer - nie bedzie możliwości sprawdzenia sieci WiFi :()
2. Skonfiguruj serwer RADIUS (freeradius2 (lub 3)). Serwer ma uwierzytelniać jedynie za pomocą protokołu EAP-PEAP-MSCHAPv2 (ew. EAP-TTLS-PAP). (10p)

Uwagi:

- Sprawdzanie listy opcja 1 (na pracowni): łączę się do skonfigurowanej sieci WiFi używając komórki z Androidem używając jako użytkownika “test” z hasłem PrASK11.
- Sprawdzanie listy opcja 2: poproszę konfigurację na maila oraz screen z informacją o szyfrowaniu połączonej sieci WiFi (jażeli jest to możliwe), dodatkowo po połączeniu VPN/SSH z Państwa PrASKowej vm ma być możliwość komunikacji z serwerem Radius za pomocą radclient. Jeżeli miejsce instalcji to PrASKowa vmka to oczywiście to będzie działać (o ile serwer RADIUS działa).

Lista potrzebnych pakietów dla radiusa (wersja 2; dla wersji 3 - analogicznie) - dotyczy tylko routerów z OpenWrt

```
freeradius2-mod-eap-mschapv2
freeradius2-mod-eap-peap
freeradius2-mod-eap-tls
freeradius2-mod-eap
freeradius2
freeradius2-mod-mschap
freeradius2-mod-files
freeradius2-utils
```

Do tego należy zmienić sam pakiet access pointa (hostapd) z wersji domyślnej, która nie wspiera uwierzytelniania RADIUS (pakiet “wpad-mini”) na taką która wspiera (pakiet “wpad”), 
o ile w obrazie nie ma już odpowiedniej wersji…

```
opkg list-installed |grep wpad
```

Obraz OpenWrt na routerach PrASKowych ma wbudowaną pełną wersję hostapd (pełny wpad).

