/*
-> Czy wartości typów prostych też mają prototypy?
Tak, ale nie. Wartości typów prostych nie mają prototypów, ale mają obiekty opakowujące, które mają prototypy.

-> Czy to znaczy że one są obiektami i można im dodawać dowolne pola/funkcje, jak wszystkim obiektom?
Nie, wartości typów prostych nie są obiektami i nie można im dodawać dowolnych pól ani funkcji, 
jednak mają one metody podobne do tych z obiektów, ponieważ mają powiązane obiekty opakowujące.
Te obiekty opakowujące są tworzone automatycznie, gdy metoda jest używana na wartości typu prostego,
po czym są usuwane. Proces ten nazywa się "boxing" lub "wrapping".
*/

var n = 1;
// liczba ma prototyp?
console.log(typeof Object.getPrototypeOf(n));       // object
console.log(Object.getPrototypeOf(n));              // null prototype
var obj = {}
console.log(typeof Object.getPrototypeOf(obj));     // object
// można jej dopisać pole/funkcję?
n.foo = 'foo';
console.log(n.foo);             // undefined

/*
Ostatnia linia programu próbuje wyświetlić wartość właściwości foo dla zmiennej n. 
Jednakże, ponieważ n jest wartością typu prostego (liczbą), a nie obiektem, 
to metoda do niej przypisywana, jest tak na prawdę przypisana do tymczasowego obiektu opakowującego,
który jest od razu usuwany razem z metodą.
*/
