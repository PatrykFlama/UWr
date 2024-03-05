







## Wykład 3
* getters, setters
* interface

* `delegate` - wzorzec dla funkcji
* `event` - lista zdarzeń (wywoływanych funkcji danego typu)

```cs
class Person {
    // defaultowe gettery i settery:
    public string Name { get; set; }

    // są równoważne z
    private string _name;
    public string Name {
        get { return _name; }
        set { _name = value; }
    }
}
```

##### Mechanizm refleksji i atrybutów
metoda `InvokeMember`

```cs
// zał że mamy klasę Person
Person p;

// weźmy typ Person
Type t = p.GetType();
t.InvokeMember("Fun", BindingFlags.InvokeMethod, null, p, new object[] { 1, 2 });
t.InvokeMember("Fun", BindingFlags.InvokeMethod, null, p, new object[0]);   // bez argumentów
```

Przykładowe zastosowanie - chcemy napisać funkcję przekształcającą dowolny obiekt na plik JSON  

inne refleksje i przykłady:
* `GetProperties`


```cs
Type ot = object.GetType();
foreach(PropertyInfo info in ot.GetProperties(...)){
    info.Name;
    info.GetValue...
}
```

atrybuty:  
[Omit], OmitAttribute, typeof(OmitAttribute), if(info.OmitAttribute...), [AtturbuteUsage(AttributeTargets.Property())]
