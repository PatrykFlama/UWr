using System;

/*
Właściwość z polem kopii zapasowej zawiera osobno zdefiniowane pole, które przechowuje wartość właściwości. Wartość właściwości jest odczytywana i ustawiana za pośrednictwem tego pola. Oto przykład:
*/
public class Person
{
    private string _name; // Pole kopii zapasowej

    // Właściwość z polem kopii zapasowej
    public string Name
    {
        get { return _name; }
        set { _name = value; }
    }
}
/*
W tym przykładzie _name jest polem kopii zapasowej dla właściwości Name. Jest ono używane, aby przechowywać i zwracać wartość Name.

]Właściwość implementowana automatycznie to skrócona forma definicji właściwości, w której kompilator automatycznie tworzy pole kopii zapasowej. Oto jak to wygląda:
*/
public class Person
{
    // Właściwość implementowana automatycznie
    public string Name { get; set; }
}

/*
W tym przypadku, kompilator automatycznie tworzy pole kopii zapasowej dla właściwości Name, więc nie musisz jawnie deklarować tego pola. Jest to bardziej zwięzła składnia, która nadal zapewnia dostęp do wartości właściwości.
*/




