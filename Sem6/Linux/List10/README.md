[(back)](../)

# List 10
| 1 | ... |
|---|-----|
| X |     |


# Zad 1
## Pakiety i narzędzia
### `kbd` ([kbd-project.org](kbd-project.org))
Zawiera narzędzia do konfigurowania układu klawiatury i czcionek w konsoli tekstowej  
Ciekawsze programy: 
- `setfont` – ładuje czcionkę do konsoli   
- `loadkeys` - ładowanie map klawiatury
- `dumpkeys` – wyświetla mapę klawiatury
- `showconsolefont` – wyświetla aktualnie używaną czcionkę konsoli

### `console-tools` ([lct.sourceforge.net](lct.sourceforge.net))
starszy, mniej rozbudowany     
obsługuje podobne funkcje jak kbd, ale obecnie większość dystrybucji (w tym Debian) korzysta z kbd  


### `console-setup` 
plik konfiguracyjny dla `setupcon`  
konfiguruje czcionki, mapy klawiatury i ustawienia terminala  
domyślnie wczytuje czcionkę i mapę klawiatury przy starcie  
znajduje się w `/etc/default/console-setup`

```ini
CHARMAP="UTF-8"
CODESET="Lat15"
FONTFACE="Terminus"
FONTSIZE="16x32"
```
Przykładowe czcionki: Fixed, VGA, Terminus, Lat2-Terminus


## Programy
### `setfont` – ładowanie czcionek konsoli
```bash
setfont /usr/share/consolefonts/Lat2-Terminus16.psf.gz
```

Lokalizacje czcionek:  
- /usr/share/consolefonts/ – czcionki w formacie .psf.gz
- /etc/default/console-setup – plik konfiguracyjny
- /etc/console-setup/cached.kmap.gz – mapa klawiatury


### `setupcon` – konfiguruje ustawienia konsoli według pliku `/etc/default/console-setup`
```bash
setupcon            # zastosuj bieżącą konfigurację
sudo setupcon --save    # zapisz do cache
sudo update-initramfs -u  # aktualizuje initramfs
```
zapisanie do cache tworzy plik `/etc/console-setup/cached_XXX.kmap.gz`


## Formaty
### PSF (PC Screen Font)	
format używany przez Linuksa w trybie tekstowym; obsługiwany przez setfont  
PSF1 i PSF2 (z obsługą map znaków Unicode)  

### BDF (Bitmap Distribution Format)	
format rastrowy czcionek wykorzystywany przez X11 (system graficzny dla uniksów), łatwy do edycji  

### PCF (Portable Compiled Format)	
skompilowana wersja czcionki BDF używana przez X serwer  
wydajniejszy niż BDF, trudniejszy do edycji 

### Narzędzia do konwersji
`pcf2bdf` – konwertuje czcionki PCF do formatu BDF 
```bash
pcf2bdf myfont.pcf > myfont.bdf
```
`bdf2pcf` – konwertuje czcionki BDF do formatu PCF
```bash
bdf2pcf myfont.bdf > myfont.pcf
```

`bdf2psf` – konwertuje czcionki BDF do formatu PSF (używanego w konsoli):  

```bash
bdf2psf myfont.bdf 512 > myfont.psf
```


## Urządzenia i narzędzia terminalowe
### `/dev/tty*` (teletypewriter device)
terminale tekstowe

### `/dev/vcs*
dostęp do treści aktualnie wyświetlanej w wirtualnych konsolach (tekst)

### `/dev/fb*`
framebuffer – dostęp do bufora ramki (pikselowego obrazu ekranu) 

### Programy
- `fbset` – konfiguruje rozdzielczość framebuffera
- `fbgrab`, `fbcat` – wykonują zrzuty ekranu z framebuffera
- `fbi` – przeglądarka obrazów w konsoli framebufferowej
- `chvt` – przełącza między terminalami (chvt 1 -> tty1)
- `openvt` – uruchamia program na innym terminalu


## Framebuffer, KMS, hardware text mode
### Framebuffer (fbdev)
mechanizm jądra umożliwiający dostęp do pamięci ekranu w trybie graficznym (nawet bez X11) 

### Kernel Mode Setting (KMS)	
ustawienie rozdzielczości i trybu ekranu w jądrze zamiast przez użytkownika  
czyli kernel potrafi już rozmawiać z kartą graficzną

### Hardware Text Mode	
tryb sprzętowy, w którym tekst renderowany jest przez BIOS/kartę graficzną (np. VGA). szybki, ale ograniczony


## Dlaczego w konsoli tekstowej nie można używać czcionek wektorowych i antialiasingu?
- konsola tekstowa (tty) pracuje w trybie rastrowym, nie ma bibliotek do wytwarzania rastru z wektorowego
- nie obsługuje grafiki ani przez GPU, ani przez biblioteki font rendering (np. FreeType)
- antialiasing i czcionki wektorowe wymagają znania kolorów terminala, zmiany kolorów pikseli, etc
- (terminale graficzne działają na np X11 i mogą korzystać z czcionek TTF/OTF i antialiasingu)


## Konfiguracja czcionki w initramfs
### Generowanie czcionki
w `/etc/default/console-setup` ustawiamy czcionkę, np
```ini
FONTFACE="Terminus"
FONTSIZE="24x12"
CHARMAP="UTF-8"
```

generujemy czcionkę
```bash
sudo setupcon --save
```

powinno utworzyć plik `/etc/console-setup/cached.kmap.gz`

### Dodanie hooka do initramfs
tworzymy plik `/etc/initramfs-tools/hooks/z_cfont`
```bash
#!/bin/sh
set -e


# upewnij się, że setfont jest dostępny
if [ ! -x /bin/setfont ]; then
    echo "setfont is missing; please install the 'kbd' package."
    exit 0
fi

. /usr/share/initramfs-tools/hook-functions

# skopiuj binarkę setfont
copy_exec /bin/setfont /bin

# utwórz katalog i skopiuj czcionkę
mkdir -p "${DESTDIR}/etc/console-setup/"
cp /etc/console-setup/cached_Terminus24x12.psf.gz \
   "${DESTDIR}/etc/console-setup/"

exit 0
```

i dodajemy prawa do wykonywania
```bash
sudo chmod +x /etc/initramfs-tools/hooks/z_cfont
```


### Skrypt init top
tworzymy plik `/etc/initramfs-tools/scripts/init-top/z_cfont`
```bash
#!/bin/sh
set -e

# ścieżka do czcionki w initramfs
FONT="/etc/console-setup/cached_Terminus24x12.psf.gz"

# jeśli jest setfont i plik czytelny, załaduj na tty1–tty6
if [ -x /bin/setfont ] && [ -r "$FONT" ]; then
    for N in {1..6}
    do
        setfont -C /dev/tty$N "$FONT" || true
    done
fi

exit 0
```
i dodajemy prawa do wykonywania
```bash
sudo chmod +x /etc/initramfs-tools/scripts/init-top/z_cfont
```


### Aktualizacja initramfs
```bash
sudo update-initramfs -u
```



____


# Notes
## Zad 1
kiedyś jądro się nie zajmowało czcionkami, bo karta graficzna miała wbudowane czcionki - więc jądro tylko przesyłało do niej tekst  
ale teraz sie tak nie robi, teraz mamy tylko framebuffer i jądro musi się zajmować czcionkami  

- klikając w klawiaturę wysyłamy `scancode`'y (`iftest` - narzędzie wyświetlające kody skanowania)  
- kody są odbierane przez jądro i wytwarzane są `keycode`'y (w kernelu)
- `keycode`'y są mapowane za pomocą `keymap` 
- ...
- wytwarzane są znaki unicode (UTF-8) i wysyłane do aplikacji


### `chvt` – zmiana terminala
```bash
chvt 1
```
przełącza na terminal 1 (tty1)

### jak sprawdzić czy tty jest żywy?
brzydka sztuczka - sprawdzamy czy jego vcs istnieje (`ls /dev/vcs*`)  


## Zad 2
'wszystko co niezaszyfrowane, jest podatne' 

`lsblk -f` - wypisuje wszystkie urządzenia blokowe i ich informacje

## Zad 4
porzebujemy `init`a oraz `bin`a (jądro odpali inita, a bin jest potrzeby żeby 'mieć co robić')  
w naszym binie chcemy `busybox`a  

w inicie (który jest odpalany przez `excve /bin/sh`) przede wszystkim potrzebujemy mountów naszego initramfs  

wszystko uruchomimy za pomocą grub'a (musimy stworzyć custom config)

żeby dalej to rozwinąć możemy dodać jakieś moduły jądra do `lib`    


## Zad 10
'gentoo - wszystkie potrzebne moduły wkompilowane w kernel'  

