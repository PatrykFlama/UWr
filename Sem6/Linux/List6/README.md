



# Lista 6

| 1 | 2 | 3 | 4 | 5 | 6 | 7 |
|---|---|---|---|---|---|---|
| 1 | 2 | 1 | 1 | 1/2 |   |   |

## Zad 2
[chroot tutorial](https://www.turnkeylinux.org/docs/chroot-to-repair-system)  
grub install specify efi location with `--boot-directory` option

## Zad 3
### Tryby dostępu (rwx) dla katalogów
- r (read, 4) – pozwala na wylistowanie zawartości katalogu (ls), ale nie umożliwia otwierania plików
- w (write, 2) – pozwala na tworzenie, usuwanie i zmianę nazw plików w katalogu
- x (execute, 1) – pozwala na dostęp do plików w katalogu, ale bez listowania (ls)

np `chmod u+rwx katalog` - dodaje uprawnienia rwx dla właściciela katalogu

### Sticky bit (t)
- dla katalogów: zapobiega usuwaniu/zmianie nazwy plików przez innych użytkowników, jeśli nie są ich właścicielami
- dla plików na niektórych starszych systemach: bit zapisuje obraz tekstu w swapie, aby szybciej ładować programy

np `chmod +t katalog` - dodaje sticky bit do katalogu

### Bit ustawiania grupy (setgid, g+s)
- dla katalogów: nowe pliki dziedziczą grupę katalogu zamiast grupy tworzącego użytkownika
- dla plików wykonywalnych: proces uruchomiony z tym plikiem działa z uprawnieniami grupy pliku

np `chmod g+s katalog` - dodaje bit setgid do katalogu

### Prawa dostępu dla `/tmp` i `/usr/local` w Debianie
- `/tmp`: `drwxrwxrwt` (777 + sticky bit t)

każdy może tworzyć pliki, ale może usuwać tylko swoje

- `/usr/local`: `drwxr-xr-x` (755)

root może zapisywać, inni mogą tylko odczytywać i wchodzić do katalogu


## Zad 4
- znajdźmy nasz pendrive w systemie za pomocą `lsblk`
- zaczniemy od przygotowania pendrivea za pomocą `fdisk`, 
- następnie sformatujemy go do systemu plików FAT32 oraz sprawzimy czy etykieta została nadana:

```bash 
sudo mkfs.vfat -F 32 -n MY_PRECIOUS /dev/sda1
sudo blkid /dev/sda1
```

- tworzymy katalog montowania, skorzystamy z opcji `-m 0` która umożliwia dostęp do katalogu tylko jeżeli został on zamontowany:
```bash
sudo mkdir -m 0 /media/my_precious/
```

- dodajemy wpis do `/etc/fstab`, skorzystamy z nadanej etykiety:
  - `LABEL=MY_PRECIOUS` - montowanie po etykiecie zamiast nazwy urządzenia
  - `vfat` - system plików FAT
  - `user` - zwykły użytkownik może montować
  - `noauto` - system nie montuje automatycznie
  - `uid=1000,gid=1000` - przypisuje własność użytkownikowi o UID=1000 (domyślny pierwszy użytkownik)
  - `umask=077` - katalogi `drwx------`, pliki `-rw-------` (tylko właściciel ma dostęp)
  - `noexec` - blokuje uruchamianie programów z pendrivea

```bash
LABEL=MY_PRECIOUS  /media/my_precious  vfat  user,noauto,uid=1000,gid=1000,umask=077,noexec  0  0
```

- tearzaz możemy montować nasz pendrive:
```bash
mount /media/my_precious/
umount /media/my_precious/
```

## Zad 5
będziemy korzystać z `dd` do testowania wydajności dysków, konwertuje on i kopiuje pliki, możemy użyć go do testowania wydajności dysków.

### tworzenie ramdysku:
```bash
sudo mount -t tmpfs -o size=2G tmpfs /mnt/ramdisk
df -h /mnt/ramdisk
```

### tworzenie zaszyfrowanego kontenera na ramdysku
```bash
dd if=/dev/urandom of=/mnt/ramdisk/container.img bs=1M count=1024 status=progress
```

podłączanie kontenera jako urządzenie blokowe:
```bash
sudo losetup /dev/loop99 /mnt/ramdisk/container.img
```

[szyfrowanie kontenera](https://askubuntu.com/questions/599044/luks-and-loop-device):
```bash
sudo cryptsetup luksFormat /dev/loop99
sudo cryptsetup open /dev/loop99 ramdisk_enc
sudo mkfs.ext4 /dev/mapper/ramdisk_enc
sudo mount /dev/mapper/ramdisk_enc /mnt/ramdisk_enc
```
- inicjalizacja zaszyfrowanego kontenera
- otwieramy kontener i przypisujemy mu urządzenie blokowe `/dev/mapper/ramdisk_enc`
- formatujemy kontener do systemu plików ext4
- montujemy kontener w `/mnt/ramdisk_enc`

### test zapisu na ramdysku
```bash
dd if=/dev/urandom of=/mnt/ramdisk_enc/testfile bs=1M count=1024 conv=fsync status=progress
```

### test na innych dyskach
wypisujemy dyski:
```bash
lsblk
```

powtarzamy zapis:
```bash
dd if=/dev/urandom of=DRIVE_LOCATION/testfile bs=1M count=1024 conv=fsync status=progress
```

### test odczytu
```bash
dd if=/mnt/ramdisk_enc/testfile of=/dev/null bs=1M status=progress
```


