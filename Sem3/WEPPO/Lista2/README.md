[(wróć)](../)

# Lista 2
| 1 | 2 | 3 | 4 |
|---|---|---|---|
| X | ~ | X | X |

## NPM
`-g` - instalacja globalna, w pp instaluje lokalnie do projektu

zaczynamy od `npm init` - tworzy plik `package.json` z danymi o projekcie  
* aby instalować pakiety oraz dodawać zależności od razu do `package.json` używamy `npm install [nazwa pakietu] --save`  
* aby odtworzyć zależności z pliku `package.json` korzystamy z `npm install` (powstaje folder `node_modules`)
* aby usunąć zależności z `package.json` korzystamy z `npm uninstall [nazwa pakietu] --save`

## Zadanie 2
* `.` vs `[]` 
do [] podajemy string do . podajemy nazwę zmiennej
* argumenty różne od stringa do []
    * liczba - zamieniana na stringa
    * inny obiekt - zamieniany na stringa `'[object Object]'` (typ obiektu)
    * wpływ na klucz - różne liczby to różne stringi sprawdzić zachowanie przy toString
* arg inny niż number
    * string - próba konwersji na liczbę, następnie undefined
    * obiekt - undefined
    * wartość zostaje dodana do tablicy i jest dostępna pod podanym kluczem (nie jest to liczone do długości tablicy) (zostanie to dodane to do obiektu, zamiast tablicy???)
    * można nadpisać długość tablicy, ale zbędne elementy zostaną usunięte, a dodatkowe ustawione jako puste

## Zadanie 3
```js
(![]+[])[+[]]

![] => false
+ => konkatencja
[] => pusty string

+[] => 0

(![]+[])[+[]] => (false+"")[0] => ("false")[0] => "f"
```

```js
(![]+[])[+!+[]] => ("false")[1] => "a"
+!+[] => +!0 => +true => 1
```

```js
([![]]+[][[]])[+!+[]+[+[]]] => ([false]+undefined)['1'+'0'] => ("falseundefined")[10] => "i"
```

```js
(![]+[])[!+[]+!+[]] => ("false")[true + true] => ("false")[2] => "l"
```


## Zadanie 4
Typeof zwraca stringa, który reprezentuje typ zmiennej. Działa tylko dla obiektów prostych.  
Natomiast instanceof sprawdza czy dany obiekt jest instancją danej klasy.  

