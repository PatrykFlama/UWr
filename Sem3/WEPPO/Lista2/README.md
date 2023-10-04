[(wróć)](../)

# Lista 2
| 1 | 2 | 3 | 4 | 5 |
|---|---|---|---|---|
|   |   |   |   |   |

## NPM
`-g` - instalacja globalna, w pp instaluje lokalnie do projektu

zaczynamy od `npm init` - tworzy plik `package.json` z danymi o projekcie  
* aby instalować pakiety oraz dodawać zależności od razu do `package.json` używamy `npm install [nazwa pakietu] --save`  
* aby odtworzyć zależności z pliku `package.json` korzystamy z `npm install` (powstaje folder `node_modules`)
* aby usunąć zależności z `package.json` korzystamy z `npm uninstall [nazwa pakietu] --save`

## Zadanie 2
* `.` vs `[]` 
```js
var obj = { name: "John" };
var str = "name";
console.log(obj.str); // undefined
console.log(obj[str]); // John
```

* 