(wróć)[../]
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|---|---|---|---|---|---|---|---|
| X | X | X | X | X | X | X | X |


## Zadanie 4
elementy Application mają zasięg całej aplikacji   
elementy Session mają zasięg sesji użytkownika  
elementy Item mają zasięg jednego zapytania  

elementy z aplikacji muszą być zabezpieczone przed wielowątkowością - wielu użytkowników może być połączonych na raz i próbować uzyskać dostęp do tego samego obiektu  
elementy z sesji mogą wymagać dodatkowych zabezpieczeń (ale najczęściej nie wymagają)  
elementy z items nie wymagają zabezpieczenia przed wielowątkowością

kontenera application mogłoby nie być w interfejsie programistycznym, ponieważ odpowiada on statycznym zmiennym

## Zadanie 5
powinniśmy skorzystać z kontenera `Items` ponieważ odpowiadałby onn zakresowi jednego żądania  

wykorzystanie kontenera `Application`  doprowadziłoby do problemów z wielowątkowością, wiele zapytań korzystałoby z tego samego połączenia z bazą  
dodatkowo z racji iż obiekt byłby współdzielony między zapytaniami => użytkownikami, pojawiłyby się problemy z bezpieczeństwem danych  

wykorzystanie kontenera `Session` doprowadziłoby do utworzenia nowego połączenia na każdego istniejącego urzytkownika, bez względu na to czy jest on aktywny czy nie, więc mielimyśmy dużą liczbę połączeń z bazą, z czego większość nie byłoby aktywnie wykorzystywanych

## Zadanie 6
gdy asp net wykryje w katalogu plik `app_offline.htm` zacznie on proces bezpiecznego zamknięcia aplikacji  
będzie on zatrzymywać przetwarzanie żądań przychodzących, wysyłając im jako odpowiedź zawartość tego pliku  
po zdefiniowanym czasie proces aplikacji zostanie całkowicie zatrzymany

## Zadanie 7
Specyfikacja `Content-Disposition`

* `attachment` lub `inline` - sposób 'pokazania' pliku
* specyfikacja RFC5987 dla nazw plików w UTF8  
```cs
string fileName = "my_file.xml";
Response.AddHeader("Content-Disposition", $"attachment; filename*=UTF-8''{Uri.EscapeDataString(fileName)}");
```
`filename*` - kodowanie w UTF8  
`Uri.EscapeDataString(filename)` koduje nazwę pliku

* jeżeli przeglądarka nie wspiera RFC5987 możemy to sprwadzić i wysłać inną wersję nazwy pliku takim przeglądarkom



# Pytania
* zad 2:
	* to jak wygląda wersja naiwna?

* Zad 4:
	* tak to powinno wyglądać?
	* dlaczego nie można lockować innych kontenerów
	* to po co ten kontener items