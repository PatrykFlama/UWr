/*
Symulujemy metody prywatne za pomocą funkcji - ponieważ nie mamy dostępu do jej wartości z zewnątrz.
Z racji iż funkcja jest obiektem, to możemy do niej dopisać właściwości, które będą dostępne tylko wewnątrz funkcji.
*/

function Foo(str = "V") {
    // prywatna zmienna
    var _v = str;

    // prywatna funkcja
    function Qux() {
        console.log(_v);
    }

    // publiczna metoda
    this.Bar = function() {
        Qux();
    };
}

// dodanie metody do prototypu
Foo.prototype.Bar = function() {
    this.Qux();
};

// tworzenie instancji obiektu Foo
var foo1 = new Foo();
var foo2 = new Foo("F");

// wywołanie publicznej metody Bar na instancjach
foo1.Bar(); // "Qux"
foo2.Bar(); // "Qux"

// próba wywołania prywatnej funkcji Qux na instancjach
// foo1.Qux(); // TypeError: foo1.Qux is not a function
