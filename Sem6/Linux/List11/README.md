# Lista 11

| 1 | 2 | 3 | 4 |
|---|---|---|---|
| X |   |   |   |


## Zad 1
### `fdisk(8)`
- klasyczne narzędzie do zarządzania tablicami partycji, działające w trybie dialogowym
- obsługuje formaty: MBR (DOS), GPT, BSD, Sun, SGI
- skupia się na podstawowej strukturze partycji (tworzenie, usuwanie, zmiana rozmiaru), bez bezpośredniej integracji z systemami plików
- optymalizuje domyślnie rozkład partycji pod kątem nowoczesnych dysków (np. 4K-sektorów)

- teoretycznie pozwala na skryptowanie za pomocą `ssfdisk(8)`

- modyfikacje istnieją w ramie, muszą zosatć zapisane manualnie

### `parted(8)`
- zaawansowane narzędzie z obsługą skryptów (np. -s dla trybu nienadzorowanego)
- obsługuje więcej formatów tablic partycji, w tym GPT, MS-DOS, BSD, aix, amiga itd
- umożliwia modyfikację systemów plików (np. zmiana rozmiaru partycji z uwzględnieniem systemu plików, choć wymaga dodatkowych narzędzi jak resize2fs)
- wspiera automatyczne wyrównanie partycji (np. --align optimal dla optymalnej wydajności)

- posiada try skryptowy
- bardziej intuicyjna składnia
- umożliwia zmianę rozmiaru partycji bez utraty danych (w połączeniu z `resize2fs` lub `resizepart`)
- pozwala określić typ systemu plików przy tworzeniu partycji (ale jest to już depreceated, są tylko pozostałości)

- zmiany są wykonywane na bieżąco na dysku

____

tldr: fdisk jest prostrzym narzędziem tylko do patrycjonowania, natomiast parted jest mocno rozbudowane dla zaawansowanych operacji

### tworzenie `disk.img` (parted)
tworzymy `disk.img`, taki że: 
976580000 sektorów * 512 B = 500008960000 B

```bash
truncate -s $((976580000 * 512)) disk.img
```

następnie partycjonujemy go za pomocą `parted` (fdisk nie pozwala na tworzenie partycji przed 2048 sektorem)

```bash
parted disk.img

mklabel msdos # tworzenie tablicy partycji MBR
unit s # ustawienie jednostki na sektory
mkpart primary fat32 63 67108863
mkpart primary ext4 67108864 486539263
mkpart extended 629145600 976579999

# partycja rozszerzona
mkpart logical ext4 629147648 713033727
mkpart logical linux-swap 975175680 976199679
quit
```

### tworzenie `disk.img` (sfdisk)
alternatywna metoda za pomocą `sfdisk` (skryptowalny `fdisk`):

```
sfdisk disk.img <<EOF
label: dos
unit: sectors

disk.img1 : start=63, size=67108801, type=0x0c
disk.img2 : start=67108864, size=419430400, type=0x83
disk.img4 : start=629145600, size=347434400, type=0x05
disk.img5 : start=629147648, size=83886080, type=0x83
disk.img6 : start=975175680, size=1024000, type=0x82
EOF
```

### tworzenie urządzenia blokowego
za pomocą `losetup` tworzymy urządzenie blokowe, polecenia `partx`/`partprobe`/`kpartx` są używane do aktualizacji systemu o nowe partycje

```bash
LOOPDEV=$(losetup --find --show --partscan disk.img)
partprobe "$LOOPDEV"
lsblk "$LOOPDEV"
```




# Some notes
w gpt są 2 kopie - z przodu oraz z tyłu, dodatkowo jest hash sha256 (więc 'ręczna' edycja w edytorze teksu nie jest prosta)  
w mbr jest to możliwe, bo jest to ~ 512 B  
fdisk pochodzi z bds unix, parted został stworzony w ramach projektu gnu  

> "unix haters handbook" - taka se książka

w mbr normalnie było tak, że w sektorach 1-62 był grub, a od 63 partycje  
standard fdisk (spowodowany pojawieniem się gpt) sprawia że między grubem a parycjami jest parwie 1MB zmarnowanego miejsca  
w xfs (np ext4) blok nie może być większy niż strona pamięci ram, czyli najczęściej 4kB - więc pamięć na dyskach powinna być wyrównana do 4kB aby nie ładować 2 stron do ramu aby odczytać/zapisać 1 sektor => w tym przypadku wyrównywanie do aż do 1MB nie ma sensu


za pomocą jakiegoś hex edytora (np `xxd`) mozemy sprawdzić zmiany na naszym disk.img  

> prinf "%d\n" 0xc2r000 | bc -l

żeby w fdisk założyć partycję przed 2048 sektorem musimy przedjść do trybu kompatybilności: w fdisk opcja `c`   


