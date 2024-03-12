[wróć](../)

# TOC
- [TOC](#toc)
- [Wykład 3](#wykład-3)
  - [Klasy i obiekty, eventy i delegaty](#klasy-i-obiekty-eventy-i-delegaty)
  - [Mechanizm refleksji i atrybutów](#mechanizm-refleksji-i-atrybutów)
- [Wykład 4](#wykład-4)
  - [Typy generyczne](#typy-generyczne)
      - [Listy](#listy)
  - [Enumeratory](#enumeratory)
  - [Rozszerzenia enumeratorów](#rozszerzenia-enumeratorów)
  - [Metody rozszerzające](#metody-rozszerzające)
    - [EnumerableExtensions](#enumerableextensions)
      - [GroupBy i var](#groupby-i-var)
  - [Klasy anonimowe](#klasy-anonimowe)


# Wykład 3
## Klasy i obiekty, eventy i delegaty
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

## Mechanizm refleksji i atrybutów
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


# Wykład 4
## Typy generyczne
Typy generyczne w cs różnią się od cpp - w samym kompilacie nadal są one typami generycznymi (cpp w trakcie kompilacji generuje wszystkie potrzebne optypowane funkcje)  
tworzy to pewien problem - cs nie wie jakie metody są dozwolone dla typu generycznego (gdzie cpp 'sprawdza' czy metody istnieją dopiero po wykorzystaniu ich z konkretnym typem)  

```cs
class C<T> {
    T x;

    public T func(T a){
        return x+a;     // error
    }
}
```

aby rozwiązać ten problem możemy korzystać z ogranicznika `where`  
```cs
class C<T> where T : IInterface {       // T musi implementować IInterface, więc wiemy że posiada jakieś metody
    T x;
}
// poprawne tez jest
class C<T> where T : class {       // T musi być klasą
    T x;
}
```

#### Listy
`ArrayList` - lista obiektów, nie jest typowana, więc możemy wrzucić tam wszystko  
`List<T>` - typowana lista

## Enumeratory
```cs
public interface IEnumerableObject {      // domyślnie ten interfejs jest już zaimplementowany w cs (IEnumerable)
    object GetCurrentValue();
    void MoveToNext();
    bool IsFinished();      // teoretycznie można to zmergowac z MoveToNext
    void Reset();
}

public class EnumerableClass : IEnumerable, IEnumerator {
    public int i;

    public EnumerableClass(){
        i = 0;
    }

    // metoda 1
    public IEnumerator GetEnumerator(){
        return this;
    }

    // metoda 2
    public IEnumerator GetEnumerator(){
        yield return 1;

        while (i < 10){
            yield return i;
            i++;
        }
    }

    public object Current {
        get{
            return i;
        }
    }

    public bool MoveNext() {
        i++;
        return i < 10;
    }

    public void Reset(){
        i = 0;
    }
}

EnumerableClass ec = new EnumerableClass();

// korzystanie z foreach
foreach(object i in ec){
    Console.WriteLine(i);
}

// jest równoważne temu
IEnumerator e = ec.GetEnumerator();
while(e.MoveNext()){
    Console.WriteLine(e.Current);
}
```

## Rozszerzenia enumeratorów
```cs
public class EnumerableExtensions {
    public static T Find<T>(this IEnumerable<T> e, Predicate<T> p){
        foreach(T i in e){
            if(p(i)){
                return i;
            }
        }
        return default(T);      // zwraca domyślną wartość dla danego typu, wymagane bo musimy zwórić ten sam typ
    }

    public static IEnumerable<T> FindAll<T>(this IEnumerable<T> e, Predicate<T> p){
        foreach(T i in e){
            if(p(i)){
                yield return i;
            }
        }
    }
}

// taka implementacja dodatkowych funkcji do enumeratorów nie pozwala na łańcuchowe wywoływanie kodu, i to nas smuci
EnumerableClass ec = new EnumerableClass();

EnumerableExtensions.FindAll(
    EnumerableExtensions.FindAll(
        EnumerableExtensions.FindAll(ec, x => x > 5),
        x => x < 10
    ),
    x => x > 7
);

// a chcielibyśmy
ec
    .FindAll(x => x > 5)
    .FindAll(x => x < 10)
    .FindAll(x => x > 7);

// jak to naprawić?
```

## Metody rozszerzające 
Aby stworzyć metodę rozszerzającą musimy: 
* stworzyć statyczną klasę
* w tej klasie stworzyć statyczną metodę, która jako pierwszy argument ma słowo kluczowe `this` i typ, który chcemy rozszerzyć

```cs
// problem - chcemy ładnie wywoływać metody z innych klas na obiektach 
string s = "ala ma kota";
s = StringExtensions.ToUpperCustom(s);
// chcielibyśmy
s = s.ToUpperCustom().ToUpperCustom();

public static class StringExtensions {
    public static string ToUpperCustom(this string s){
        return s.ToUpper();
    }

    // możemy też dodawać inne arguemnty, po pierwszym przyjmującym obiekt
    public static string ToUpperCustom(this string s, int a){
        return s.ToUpper() + a;
    }
}
```
### EnumerableExtensions
Przy dodaniu do cs metody rozszerzającej dodano cały zestaw wbudowanych funkcji rozszerzających enumeratory  
zostały one zaimplementowane w bibliotece `System.Linq`  
w VStudio możemy rozróżnić metody pochodzące z zewnętrznej klasy od tych zaimplementowanych w naszym obiekcie (mają one strzałkę w dół przy nazwie metody)  

#### GroupBy i var
tutaj przydatne będzie `var` (które inferuje typ wnikowy), bo group by zwraca bardzo skomplikowany typ, więc zapisanie go (np po 3-krotnym group by) byłoby bardzo skomplikowane
```cs
public class Person {
    public string name;
    public string surname;
    public int age;
}

List<Person> = ...;

var res =
    list
        .GroupBy(x => x.surname)
        .GroupBy(x => x.age)
        .GroupBy(x => x.name);
```

kilka ciekawych przykładów
```cs
var res = 
    list
        .Where(x => x.age > 18)
        .OrderByDescending(x => x.age)
        .ThenBy(x => x.surname)
        .ThenBy(x => x.name);
```

operacje te mają bardzo sql'owy charakter, co spowodowało wprowadzenie do cs takiej składni (która jest brzydka i nie daje żadnej przewagi - i to nas smuci)
```cs
List<int> t = new List<int> {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
var res =
    from e in t
    where e > 5
    orderby e descending
    select e;
```

## Klasy anonimowe
```cs
blah blah blah 
i => new {i = i, f = i+2}
```

