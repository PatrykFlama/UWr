



# Lista 6

| 1 | 2 | 3 | 4 | 5 | 6 | 7 |
|---|---|---|---|---|---|---|
|   |   | 1 |   |   |   |   |

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