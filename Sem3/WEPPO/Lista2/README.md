[(wróć)](../)

# Lista 2
| 1 | 2 | 3 | 4 |
|---|---|---|---|
| X | X | X | X |

## NPM
`-g` - instalacja globalna, w pp instaluje lokalnie do projektu

zaczynamy od `npm init` - tworzy plik `package.json` z danymi o projekcie  
* aby instalować pakiety oraz dodawać zależności od razu do `package.json` używamy `npm install [nazwa pakietu] --save`  
* aby odtworzyć zależności z pliku `package.json` korzystamy z `npm install` (powstaje folder `node_modules`)
* aby usunąć zależności z `package.json` korzystamy z `npm uninstall [nazwa pakietu] --save`

## Zadanie 2
* użycie operatorów `.` oraz `[]` do odwoływania się do składowych obiektu  
do `[]` podajemy string do `.` podajemy nazwę zmiennej, dzięki temu przy `[]` możemy podać dowolny string <=> nazwę (np ze spacjami, znakami specjalnymi), w tym dowolne operacje zwracające w efekcie stringa
```js
var obj = {};
obj.x = 1; // obj.x==1
obj['y'] = 2; // obj.y==2
// obj.ab+c = 3; // błąd
obj['fo'+'o'] = 4; // obj.foo==4
// obj.f00 = 3; // błąd
obj['f o0'] = 5; // niestandardowa nazwa
```

* użycie argumentów innego typu niż string dla operatora [] dostępu do składowej obiektu
    * liczba - zamieniana na stringa
    * inny obiekt - zamieniany na stringa, jeżeli jest zdefiniowana metoda toString, w pp `'[object Object]'` typ obiektu
    * wpływ na klucz - wartość będzie konwertowana na stringa
```js
var obj = { x: 1, y: 2, '0': 3 };
console.log(obj["x"]); // 1
console.log(obj[y]);   // y is not defined
console.log(obj["0"]); // 3
console.log(obj[0]); // 3

var ooobj={
    valueOf: function(){
        return "0";
    }
}
console.log(obj[ooobj]); // undefined

var whaaabj={
    toString: function(){
        return "0";
    }
}
console.log(obj[whaaabj]); // 3
```

* użycie argumentów innego typu niż number dla operatora [] dostępu do tablicy
    * string - próba konwersji na liczbę, następnie undefined
    * obiekt - undefined
    * wartość zostaje dodana do tablicy i jest dostępna pod podanym kluczem (nie jest to liczone do długości tablicy) (zostanie to dodane to do obiektu, zamiast tablicy???)
    * można nadpisać długość tablicy, ale zbędne elementy zostaną usunięte, a dodatkowe ustawione jako puste
```js
var tab = [1, 2.2, 3.33, "cztery", -5];
console.log(tab[0]); // 1
console.log(tab["2"]); // 2.2

console.log(tab["szesc"]); // undefined
tab["szesc"] = 6;
console.log(tab["szesc"]); // 1

var o1 = {
    toString: function() {
        return 3;
    }
}
console.log(tab[o1]); // "cztery"

var o2 = {
    valueOf: function() {
        return "4";
    }
}
console.log(tab[o2]); // undefined

console.log(tab.length); // 5
tab.length = 6;
console.log(tab.length); // 6
console.log(tab); // [1, 2.2, 3.33, "cztery", -5, <1 empty item>, szesc: 6]
tab.length = 10;
console.log(tab); // [1, 2.2, 3.33, "cztery", -5, <5 empty items>, szesc: 6]
tab.length = 3;
console.log(tab); // [1, 2.2, 3.33]
tab.length = 0;
console.log(tab); // [szesc: 6]
```

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
Natomiast instanceof sprawdza czy dany obiekt jest instancją danej klasy. Działa tylko dla typów złożonych.  

