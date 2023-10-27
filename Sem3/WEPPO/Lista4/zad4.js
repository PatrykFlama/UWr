/*
-> Czy wartości typów prostych też mają prototypy?
Wartości typów prostych w JavaScript (takie jak string, number, boolean, null, undefined i symbol) 
nie mają prototypów. Prototypy są zdefiniowane tylko dla obiektów, 
a wartości typów prostych nie są obiektami

-> Czy to znaczy że one są obiektami i można im dodawać dowolne pola/funkcje, jak wszystkim obiektom?
Nie, wartości typów prostych nie są obiektami i nie można im dodawać dowolnych pól ani funkcji, 
jak to jest możliwe w przypadku obiektów.

Wartości typów prostych są przechowywane bezpośrednio na stosie, a nie w pamięci dynamicznej, 
jak to jest w przypadku obiektów. Wartości te są przekazywane przez wartość, a nie przez referencję, 
co oznacza, że kopiowanie wartości typu prostego tworzy nową wartość, a nie tylko referencję 
do istniejącej wartości, jak to jest w przypadku obiektów.
*/

var n = 1;
// liczba ma prototyp?
console.log(typeof Object.getPrototypeOf(n));
// można jej dopisać pole/funkcję?
n.foo = 'foo';
console.log(n.foo);
