var Person = function (name, surname) {
    this.name = name;
    this.surname = surname;
}

var Worker = function (name, surname, age) {
    Person.call(this, name, surname);
    this.age = age;
}

Worker.prototype = Object.create(Person.prototype);

/*
W powyższym kodzie, łańcuch prototypów jest ustawiany w taki sposób, że prototypem obiektu Worker 
jest obiekt Person.prototype. Dzięki temu, obiekt Worker dziedziczy właściwości i metody z prototypu Person, 
co pozwala na uniknięcie duplikacji kodu i ułatwia jego utrzymanie.

W przypadku ustawienia prototypu Worker na Person.prototype, obiekt Worker będzie współdzielił 
ten sam prototyp z obiektem Person. Oznacza to, że zmiana prototypu Worker będzie wpływała również na 
prototyp Person, co może prowadzić do nieoczekiwanych błędów.

W przypadku ustawienia prototypu Worker na nowy obiekt Person, tworzony jest nowy obiekt Person, 
który nie jest powiązany z oryginalnym prototypem Person. Oznacza to, że zmiany wprowadzone w prototypie Person 
nie będą miały wpływu na obiekt Person, który jest prototypem Worker. 
Ponadto, tworzenie nowych obiektów Person w ten sposób jest kosztowne pod względem wydajności.

W prawidłowym podejściu, prototyp Worker jest ustawiany na nowy obiekt, 
który został utworzony przy użyciu Object.create(Person.prototype). 
Dzięki temu, prototyp Worker dziedziczy właściwości i metody z prototypu Person, 
ale jest to zupełnie nowy obiekt, który nie jest powiązany z oryginalnym prototypem Person. 
W ten sposób, zmiany wprowadzone w prototypie Worker nie wpłyną na prototyp Person, 
a tworzenie nowych obiektów Person nie jest konieczne.
*/

