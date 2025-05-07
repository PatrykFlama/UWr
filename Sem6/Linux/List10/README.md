[(back)](../)

# List 10
| 1 | ... |
|---|---|
|   |   |


# Zad 1
## Pakiety i narzędzia
### `kbd` (kbd-project.org)
Zawiera narzędzia do konfigurowania układu klawiatury i czcionek w konsoli tekstowej  
Kluczowy program: `setfont` – ładuje czcionkę do konsoli   
Inne narzędzia: `loadkeys`, `dumpkeys`, `showconsolefont`  

### `console-tools` (lct.sourceforge.net)
starszy, mniej rozbudowany   
Projekt alternatywny wobec kbd, dziś rzadko używany  
Obsługiwał podobne funkcje jak kbd, ale obecnie większość dystrybucji (w tym Debian) korzysta z kbd  

### `console-setup`
Konfiguruje czcionki, mapy klawiatury i ustawienia terminala  
Domyślnie wczytuje czcionkę i mapę klawiatury przy starcie  

Główne pliki konfiguracyjne:  
- `/etc/default/console-setup`
- `/etc/console-setup/cached.kmap.gz`


## Programy
### `setfont` – ładowanie czcionek konsoli (format PSF)
```bash
setfont /usr/share/consolefonts/Lat2-Terminus16.psf.gz
```

### `setupcon` – konfiguruje ustawienia konsoli według pliku `/etc/default/console-setup`
```bash
setupcon
```


## Formaty
### PSF (PC Screen Font)	
Format używany przez Linuksa w trybie tekstowym; obsługiwany przez setfont.	  
PSF1 i PSF2 (z obsługą map znaków Unicode).

### BDF (Bitmap Distribution Format)	
Format rastrowy czcionek wykorzystywany przez X11. Łatwy do edycji.	  

### PCF (Portable Compiled Format)	
Skompilowana wersja czcionki BDF używana przez X serwer.	 
Wydajniejszy niż BDF, trudniejszy do edycji.

### Narzędzia do konwersji
`pcf2bdf` – konwertuje czcionki PCF do formatu BDF  
`bdf2psf` – konwertuje czcionki BDF do formatu PSF (używanego w konsoli):  

```bash
bdf2psf myfont.bdf 512 > myfont.psf
```






_____




### `kbd` (https://kbd-project.org)
- Zawiera narzędzia do ładowania map klawiatury i czcionek do konsoli
- Główne narzędzia:
  - `setfont` – ładowanie czcionek konsolowych (PSF)
  - `loadkeys`, `dumpkeys` – obsługa map klawiatury

### `console-tools` (http://lct.sourceforge.net)
Strasza wersja `kbd`, bardziej simplistyczna (nie tak rozbudowana)

### `console-setup`
- Umożliwia trwałą konfigurację czcionek i map klawiatury
- Automatycznie ładowane podczas startu systemu

---

## Konfiguracja czcionek (Debian)
w `/etc/default/console-setup` możemy skonfigurować czcionki i mapy klawiatury
```ini
FONTFACE="Terminus"
FONTSIZE="16x32"
CHARMAP="UTF-8"
CODESET="Lat15"
```

Przykładowe czcionki: Fixed, VGA, Terminus, Lat2-Terminus

### 2. Zastosowanie zmian
```bash
sudo setupcon
```

### 3. Zapis do cache i dla initramfs
```bash
sudo setupcon --save
sudo update-initramfs -u
```

## setfont(8)
Narzędzie do ładowania czcionek konsolowych "na żywo".

Wymaga czcionki w formacie .psf (np. .psf.gz).

Użycie:
```bash
setfont /usr/share/consolefonts/Lat2-Terminus16.psf.gz
```

Zmiana dotyczy aktualnej konsoli i nie jest trwała po restarcie.


## setupcon(8)
Narzędzie do ładowania konfiguracji czcionek i układu klawiatury z /etc/default/console-setup.

Użycie:
```bash
setupcon           # zastosuj bieżącą konfigurację
setupcon --save    # zapisz do cache
```

Lokalizacje czcionek:  
- /usr/share/consolefonts/ – czcionki w formacie .psf.gz
- /etc/default/console-setup – plik konfiguracyjny
- /etc/console-setup/cached.kmap.gz – mapa klawiatury

