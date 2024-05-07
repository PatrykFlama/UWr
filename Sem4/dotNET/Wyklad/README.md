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
- [Wykład 6 - aplikacje okienkowe](#wykład-6---aplikacje-okienkowe)
  - [Struktura plików - koncept designera](#struktura-plików---koncept-designera)
  - [Właściwości i zdarzenia komponentów](#właściwości-i-zdarzenia-komponentów)
  - [Konwencje nazw](#konwencje-nazw)
  - [Rozmieszczanie elementów w formularzu](#rozmieszczanie-elementów-w-formularzu)
- [Wykład 7](#wykład-7)
  - [Plik konfiguracyjny aplikacji dotnet - App config](#plik-konfiguracyjny-aplikacji-dotnet---app-config)
    - [App settings](#app-settings)
    - [App manifest](#app-manifest)
  - [Multiple Document Interface](#multiple-document-interface)
    - [Jak uzyskać architekturę MDI](#jak-uzyskać-architekturę-mdi)
  - [Komponenty do prezentacji zbiorów danych](#komponenty-do-prezentacji-zbiorów-danych)
    - [ComboBox](#combobox)
  - [GDI](#gdi)
    - [Custom Control](#custom-control)
- [WPF .NET (Windows Presentation Foundation)](#wpf-net-windows-presentation-foundation)
  - [WPF](#wpf)
    - [Binding](#binding)
- [SQL Server](#sql-server)
  - [SQL Server Management Studio](#sql-server-management-studio)
    - [tworzenie nowej tabeli](#tworzenie-nowej-tabeli)
    - [Przykład](#przykład)


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

# Wykład 6 - aplikacje okienkowe
podczas tworzenia nowego projektu w VStudio możemy wybrać między aplikacją okienkową opartą o klasyczny framework .NET lub nowe środowisko .NET Core   

główna pętla aplikacji jest ukryta pod wbudowaną funkcją `Application.Run` która może przyjąć formularz główny (wtedy obsłuży ona automatycznie podstawowe funkcje, np zamykanie) lub bez argumentu, wtedy musimy manualnie skończyć aplikację (np przez `Application.Exit`)   

## Struktura plików - koncept designera
w edytorze mamy 2 tryby - designera oraz kodu  
w edytorze wizualnym możemy dodawać nowe elementy przeciągając je z toolboxa i ustawiając je w oknie aplikacji  
główna klasa aplikacji jest rozbita na 2 pliki - jeden z kodem, drugi z opisem okna (designer) zawieracjący design aplikacji, strukturę klasy i podklasami  
designer jest dwukierunkowy - generowany automatycznie z widoku kodu, ale zmiany w designerze są zapisywane w kodzie  
efektem ubocznym jest ryzyko zepsucia designera, a w efekcie aplikacji  

## Właściwości i zdarzenia komponentów
zawiera ono 2 ważne zakładki - właściwości oraz zdarzenia  
(po dwukliknięciu elementu tworzy się powiązane z nim zdarzenie, aby się go pozbyć musimy usunąć powiązaną z nim funkcję oraz w designerze przypisanie tej funkcji do elementu)  
dwukliknięcie wybranego zdarzenia utworzy adekwatną funkcję w kodzie i nas do niej przeniesie, możemy też wybrać napisaną już funkcję do zdarzenia. Aby rozróżnić jaki komponent wywołał funkcję korzystamy z pierwszego argumentu przekazanego do funkcji (sender). Drugi argument to `EventArgs` - klasa zawierająca dodatkowe informacje o zdarzeniu (np. pozycja myszki)

## Konwencje nazw
typ obiektu na prefix, akcja na suffix  (buttonRefresh)  
skrócenie nazwy typu obiektu na prefix, akcja na suffix (btnRefresh)  

## Rozmieszczanie elementów w formularzu
* umieszczane elementów w komponencie `group by`
* w oknie `properties` elementu, sekcji `layout`
dock, anchor, etc
* element `panel`
panel jest niewidocznym elementem, który pozwala na rozmieszczanie jego podelementów po analogiczny sposób (taki div w html)

# Wykład 7
## Plik konfiguracyjny aplikacji dotnet - App config
w tym pliku mamy np wersję framworku (jeżeli nie mamy pliku to tworzymy App.config)  
Po zbudowaniu aplikacji plik konfiguracyjny ma nazwę odpowiadającą kompilatowi z dodatkowym rozszrzeniem .config  
> Nazwę kompilatu możemy zmienić w ustawieniach projektu (prawym na projekt -> ustawienia)  

### App settings
Aby korzystać z niego w kodzie, do referencji (bibliotek) musimy dodać System.Configuration  

klucze:
```xml
<appSettings>
  <add key="k1" value="v1"/>
  <add key="k2" value="v2"/>
</appSettings>
```
```cs
MessageBox.Show(ConfigurationManager.AppSettings["k1"])
```

Możemy tworzyć nowe pliki konfiguracyjne: 
w głównym pliku konfiguracyjnym:
```xml
<appSettinggs configSource="./path" />
```
tworzymy nowy (zaznaczamy że ma on być przenoszony do kompilatu):
```xml
version...

<appSettings>
  <add key="k1" value="v1"/>
  <add key="k2" value="v2"/>
</appSettings>
```

### App manifest
linia `requestedExecutionLevel` mamy poziom uprawnień aplikacji (jak w systemie)  `asInvoker`/`requireAdministrator`  
jako użytkownik możemy np wymusić uruchomienie jako administrator - po to m.in. jest ten plik  

## Multiple Document Interface 
Nasza aplikacja może mieć różne architektury: główne okno to dostarczyciel funkcji / dodatkowe okna pełnią główne funkcje lub MDI  
Komponenty typu menuStrip lub timer są o tyle specjalne że nie będą wyświetlane w samej aplikacji, więc sposób ich edycji się trochę różni  
### Jak uzyskać architekturę MDI
w samym formularzu możemy zmienić właściwość `isMdiContainer`   
stwórzmy nowy formularz `var frmChild = new frmMdiChild()` i ustawmy mu właściwość `frmChild.MdiParent(oknoGlowne)`   

> Jak teraz przykazywać informacje między formularzami?

stworzymy 'globalny' event do którego wszystkie formularze będą publikować, nawet lepiej - powołamy jakąś (naszą) klasę jako kanał komunikacyjny (będziemy suffixować klasy 'EventArgs')

```cs
public class DataStore {
    // dokładnie jedno wystąpienie - tworzymy teraz jedyną instancję
    public static DataStore Instance = new DataStore();

    public event DataStoreChangeDelegate DataStoreChangeEvent;

    // metoda służąca do emisji zdarzeń nazwa od Emit/Raise
    public void RaiseDataStoreChange(object sender, DataStoreChangeEventArgs e) {
        if(this.DataStoreChangeEvent != null) {
            this.DataStoreChangeEvent();
        }
    }
}

public delegate void DataStoreChangeDelegate(object sender ,DataStoreChangeEventArgs);


public class DataStoreChangeEventArgs : EventArgs {
  public string Data {get; set; }
}
```

w frmChild
```cs
private void frmMdiCgild_Load(object sender, EventArgs e) {
    DataStore.Instance.DataStoreCgangeEvent += Instance_DataStoreChangeEvent;
}

// emitujemy zdarzenie
private void Instance_DataStoreChangeEvent (object sender, EventArgs e) {
    if(sender != this) {
        this.textBox.Text = e.Data;
    }
}

private void frmMdiChild_FormClosing(object sender, FormClosingEventArgs e) {
    // e.Cancel = true;     // aby anulować zamknięcie
}

```


## Komponenty do prezentacji zbiorów danych
ComboBox, ListView, TreeView  
### ComboBox
dropdownStyle: dropdownList => użytkownik nie może dodać własnej pozycji  
`SelectedIndex` (index wybranego elementu), `SelectedText` (text na wybraneym elemencie)  
model który wspiera tekst i odpowiadającą wartość nie jest domyślnie zaimplementowany  
tworzymy więc własną implementację klasy `ComboItem`, która ma przeciążoną metodę `toString` (bo metoda toString jest ustawiana na tekst elementu) oraz jakieś ID 
teraz możemy skorzystać z `SelectedItem` (zwraca wybrany element), rzutujemy, wyciągamy ID

## GDI
czasem chcemy manualnie narysować coś na naszym oknie, służzy do tego właściwość Apparance->Paint

możemy tworzyć własne szczotki którymi będziemy rysować `Brush b = new SolidBrush(Color.Black / SystemColors...)` 
rysowanie: `e.Graphics.DrawString("abc", this.Font, b, new Point(10, 10));`

uwaga - obiekt ten będzie zajmował jeden ze skończonej liczby slotów, trzeba go więc sprzątnąć za pomocą using i scopami

```cs
using(Brush b)
using(Font f)
{
    e.Graphics.DrawString("abc", f, b, new Point(10, 10));
}
```

aby manualnie strować częstotliwością rysowania korzystamy z `this.Invalidate();`, które wywołujemy względem własnych ticków

### Custom Control
możemy tworzyć własne elementy do wrzucenia na formularz, np custombutton
```cs
public class CustomButton : Button {
    protected override void onClick(EventArgs e) {
        MessageBoxShow("abc");
        base.OnClick(e);        // wywołanie funkcji z klasy dziedziczonej
    }
}
```

nasz custombutton powininen się po kompilacji pojawić w toolboxie, w pp musimy manualnie dopisać formant

aby nie dziedziczyć po żadnym obiekcie, możemy dziedziczyć po klasie `Control`

na wykłądzie był jeszcze przykład komponentu combo z przyciskiem i combo box, rozmiary i pozycje podkomponentów ustawialiśmy w kodzie w zależności od rozmiarów combo (za pomocą zdarzenia `Resize`)  
dodatkowo żeby z poziomu naszego customowego combo mieć dostęp do combo box w środku
```cs
public ComboBox.ObjectCollection Items {
    get ...
    set ...
}
```

# WPF .NET (Windows Presentation Foundation)
microsoft stworzył jeszcze WCF i WWF  
WPF nie jest super wygodny w użytkowaniu (design piszemy w microsoftowej modyfikacji XML - XAML), a do tworzenia aplikacji na Windows mamy oprócz WinForms też **Electron** (np VScode), **Tauri**; więc WPF nie jest polecany do aplikacji desktopowych i nikt go zbytnio nie chce używać   
Nowym wymysłem microsoft było **UWP** (universal windows platform), które pozwalało na pisanie aplikacji zarówno na windowsa jak i xboxa

> Xamarin  
Framework do tworzenia aplikacji mobilnych, pozwala pisać w C#, korzysta z WPF. Też był dość problematyczny, aż microsoft wchłonął go do siebie i zrobił z niego **MAUI** (multi-platform app UI), które pozwala na pisanie aplikacji na platformy (windows, mac, linux, android, ios)  

> Avalonia  
**Avalonia** UI to framework do tworzenia aplikacji na windows, mac, linux, android, ios, *webassembly*  
wydaje się być bardzo przyszłościowa (ale nie wiadomo co się z nią stanie)

## WPF
zamiast pliku głównego Main.cs mamy App.xaml   
masakracja - wszystkie właściwości podajemy jak string, nie wiadomo jaki więc jest oczekiwany typ/zawartość stringa (np 4 liczyby po przecinku)  
WPF rozróżnia kontenery i elementy (każdy element musi mieć jakiś kontener):  
* StackPanel (rozkłada elementy w pionie)
* Canvas (pozwala na rysowanie elementów w dowolnym miejscu, za pomocą np `Canvas.Left="80"`)
* Grid (posiada właściwość `ShowGridLines`)
  * do definicji kolumn i wierszy korzsytamy z <Grid.ColumnDefinition> a w środku dla każdej kolmnt <ColumnDefinition Width="*"> gdzie `*` to równomierny rozkład (możemy dać `2*`) 
  * do elementów mamy udostępnione właściwości `Grid.Column="0"` i `Grid.Row="0"`
ciekawą własnością WPF jest atrybut `Content` dla każdego elementu, który pozwala wrzucić do niego inne elementy jako zawartość, np elipsa w przycisku:  

```xml
<Button Name="Button1" Click="Button_Click">
    <Button.ContentTemplate>
        <DataTemplate>
            <StackPanel>
                <Ellipse Fill="Red" Width="100" Height="100"/>
            </StackPanel>
        </DataTemplate>
    </Button.ContentTemplate>
</Button>
```

do elemtów możemy dodać content <ELEMENT.LayoutTransform>  
mamy też animacja, które mogą być wywołane triggerem i modyfikowane timerem `Storyboard`

### Binding
> tworzymy nowy model (osobny plik nowa klasa), z właściwością Name
w głównym pliku:
```cs
this._model = new Klasa();
this._model.Name = "abc";
this.DataContext = this._model;
```

teraz mamy dostęp do pól wewnątrz tej klasy w XAMLu (np `Text="{Binding Path=Imie}"`)  
za pomocą binding przypisujemy wartość poprzez refleksę (referencję)

problem: jeżeli w kodzie, już po utworzeniu obiektu w interfejsie użytkownika, zmienimy wartość pola, to nie zostanie ona zaktualizowana w interfejsie użytkownika (nie ma odświeżania)  
naprawiamy to za pomocą modelu zdarzeń `INotifyPropertyChanged`  
```cs
public class Klasa : INotifyPropertyChanged {
    private string _name;
    public string Name {
        get { return this._name; }
        set {
            this._name = value;
            this.OnPropertyChanged1("Name");            // staryszy sposób
            this.OnPropertyChanged1(nameof(Name));      // start sposób
            this.OnPropertyChanged2();                  // najlepiej
        }
    }

    public event PropertyChangedEventHandler PropertyChanged;

    protected void OnPropertyChanged1(string propertyName) {
        if(this.PropertyChanged != null) {
            this.PropertyChanged(this, new PropertyChangedEventArgs(propertyName));
        }
    }

    protected void OnPropertyChanged2(
        [CallerMemberName]
        string propertyName
    ) {
        
        if(this.PropertyChanged != null) {
            this.PropertyChanged(this, new PropertyChangedEventArgs(propertyName));
        }
    }
}
```



# SQL Server
[dokumentacja](https://learn.microsoft.com/en-us/sql/sql-server/editions-and-components-of-sql-server-2019?view=sql-server-ver16#sql-server-editions)  
SQL Server Developer edition - darmowa wersja do celów testowych i developerskich (z tego będziemy korzystać)  
SQL Server Express - darmowa wersja do małych aplikacji, do 10GB bazy danych  
SQL Server Management Studio - narzędzie do zarządzania bazą danych (z tego będziemy korzytać)

## SQL Server Management Studio  
-> W trakcie tworzenia bazy danych możemy wybrać:
* różne typy baz danych (które możemy wybrać tworząd nową db w SSMS) 
* wersję localdb, która sama zatrzyma naszą bazę danych przy dłuższym braku aktywności (nie zżera zasobów)  
* sposób uwierzytelniania
  * Windows Authentication - korzystamy z konta windowsowego (system kerberos)  
  
-> Właściwości instalacji samego serwera (zakładka properties, po zmianie ustawień trzeba zerstartować serwer)

> 'pliki' bazy danych  
server->logins - użytkownicy
databases->system databases - domyślne serwerowe bazy danych (domyślnie łączymy się z bazą danych master)
databases->[ nazwa ]->tables - zbiory danych
databases->[ nazwa ]->security->users - użytkownicy przypisani do bazy danych (możemy nimi zarządzać w oknie properties)

> wyróżniamy 2 typy kwerend: administracyjne oraz zapytania (np `SELECT * FROM sys.tables`)  

### tworzenie nowej tabeli
typy danych:
* char(n) - stała długość n znaków, zapisana bezpośrednio we wierszu (zawsze zajmuje dokładnie n znaków)
* varchar(n) - zmienna długość n znaków, zapisana w osobnym miejscu, przez referencję

typy dla identyfikatorów:
* int - 4 bajty
* long - 8 bajtów
* uniqueidentifier - GUID (128 bitów)  
w zakładce properties możemy nadać automatyczne tworzenie identyfikatora

pułapka: dla identyfikatora _int_ - za mało np dla tranzakcji bankowych (bodajże allegro miało z tym problem); więc _long_ brzmi sensownie, ale gdybyśmy mieli dwie bazy danych A i B, które korzystają z _long_ i byśmy chcieli je zmergeować, istnieje spore prawdopodobieństwo że identyfikatory będą się powtarzać, i trzebaby przejść przez trudny proces remapowania identyfikatorów, które kolidują; wtedy _GUID_ brzmi tym sensowniej  

identyfikatory są klastrowane - na dysku leżą wszystkie koło siebie  

klucze obce (foreign keys) ustawiamy w databases->[ baza danych ]->tables->[ tabela ]->keys  


### Przykład
tworzymy nowy projekt _console app (.net framework)_   
[strona zbierająca referencje ciągów połączeń do baz danych](https://www.connectionstrings.com/)
nawiązanie połączenia z bazą danych:  

```cs
try {
    // korzystamy z using, aby nasze połączenie zostało zamknięte po wyjściu z bloku
    using ( SqlConnection conn = new SqlConnection (
        @"server=.\sql2019;database=dbname;integrated security=true;trust_server_certificate=true;")
    ) {
        conn.Open();
        Console.WriteLine("Connected to database");
    }
} catch (Exception e) {
    Console.WriteLine(e.Message);
}
```

jednak wielokrotne łączenie się z bazą danych jest niewydajne (wielokrotne uwierzytelniane, etc)  
więc jest wbudowany proces _pooling_, który zapamiętuje ostatnie połączenie i je wykorzystuje (jeżeli nie jest zbyt stare)  
nie zawsze warto zadawać pytania do baz danych asynchronicznie, więc udostępnione są 2 typy metod zadawania zapytań  

```cs
try {
    List<Person> people = new List<Person>();

    using ( SqlConnection conn = new SqlConnection (
        @"server=.\sql2019;database=dbname;integrated security=true;trust_server_certificate=true;")
    ) {
        conn.Open();

        using (var command = new SqlCommand("..."))
        using (var reader = command.ExecuteReader()) {
            while (reader.Read()) {
                Console.WriteLine(reader.GetString(0)); // indeks kolumny
                Console.WriteLine(reader["column_name"]);

                Person person = new Person();
                person.Name = reader["name"];       // już skonwertowane na typy platformy dotnet
                person.Surname = reader["surname"];
                person.ID = reader["ID"];
                people.Add(person);
            }
        }

        // tutaj mamy gotowe List<Person>
    }
} catch (Exception e) {
    Console.WriteLine(e.Message);
}

// .......
class Person {...}
```
