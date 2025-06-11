# List 15

## Zad 2
### Opcje `make2fs`
- `-t` - typ
- `-b` - rozmiar bloku
- `-I` - rozmiar inode
- `-i` - ustaw stosunek bajty/inode'y
- `-L` - etykieta
- `-U` - UUID
- `-E` - ustawianie rozszerzonych funkcji
- `-O` - ustawienie funkcji
- `-D` - zapisuje bezpośrednio do I/O (bez buforowania)
- `-j` - stwórz system plików z journalem ext3
- `-J` - opcje journala ext3
- `-n` - tryb sandbox (wyświetla zmiany ale ich nie wykonuj)
- `-m` - ile miejsca zarezerwować dla roota

### `mke2fs.conf(5)`
zawiera domyślne ustawienia dla `mke2fs` oraz typy systemów plików i konfiguracje dla nich  
znajduje się w `/etc/mke2fs.conf  

### tworzenie zamazanego dysku 
- `sudo cryptsetup open --type=plain --key-file=/dev/urandom disk.img tmpdisk`  
  - `cryptsetup` tworzy mapowanie szyfrowane na pliku disk.img
  - `--key-file=/dev/urandom` używa losowy klucz do szyfrowania
  - tmpdisk to nazwa urządzenia w `/dev/mapper/tmpdisk`

- `sudo dd if=/dev/zero of=/dev/mapper/tmpdisk bs=1M oflag=direct conv=fsync status=progress`
  - `dd` nadpisuje cały zaszyfrowany plik zerami
  - `oflag=direct` omija buforowanie systemu plików
  - `conv=fsync` wymusza zapisy na dysk (z `fsync()` po zakończeniu)

- `sudo cryptsetup close tmpdisk` - zamyka mapowanie

### porównanie szybkości
za pomocą cryptsetup: ~173 MBps  
zapisanie dysku korzystając z generatora losowego ~135 MBps  
`dd if=/dev/urandom of=disk.img bs=1M count=1024 oflag=direct conv=fsync status=progress`

zapisanie dysku zerami ~1.4 GBps
`dd if=/dev/urandom of=disk.img bs=1M count=1024 oflag=direct conv=fsync status=progress`


### zakładanie systemu plików
`sudo mke2fs -t ext2 -m 0 -L "NiceLabel" disk.img`



## Zad 3

```bash
dd if=disk.img bs=1 skip=1024 count=1024 2>/dev/null | hexdump -C | less
```

znajduje się tutaj superblok  
znaczenia kolejnych bajtów zgodnie z https://www.kernel.org/doc/html/latest/filesystems/ext4/globals.html#super-block  

odczytanie tych informacji za pomocą dumpe2fs/sleuthkit
```bash
dumpe2fs disk.img
fsstat disk.img
```

