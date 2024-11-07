# TOC
- [TOC](#toc)
- [Wykład 3](#wykład-3)
  - [Czasy życia obiektu: współdzielony/ulotny](#czasy-życia-obiektu-współdzielonyulotny)
  - [Trzeci czas życia w aplikacjach webowych: czas życia żądania](#trzeci-czas-życia-w-aplikacjach-webowych-czas-życia-żądania)
  - [Jak się łączyć do bazy danych](#jak-się-łączyć-do-bazy-danych)
  - [Jakiej technologii użyć do dostępu do bazy danych?](#jakiej-technologii-użyć-do-dostępu-do-bazy-danych)
  - [Gdzie jest koniec cyklu życia naszego połączenia z bazą danych?](#gdzie-jest-koniec-cyklu-życia-naszego-połączenia-z-bazą-danych)
    - [Global Application Class](#global-application-class)
    - [Abstract Base Page](#abstract-base-page)
  - [Contener Dependency Injection](#contener-dependency-injection)
- [Wykład 5 - MVC](#wykład-5---mvc)



# Wykład 3
przystawka `mmc` (odpalana za pomocą aplikacji run wpisując `mmc`) pozwala na sprawdzenie wersji sql server oraz baz danych na maszynie   
w sql `n` oznacza znak w UTF8 (np typ nvarchar lub N'ąćęł')    
w sql sposób sortowanie musi być ustalony w trakcie tworzenia kolumny (później nie da się go zmienić)  
skrypty do migracji bazy danych powinny być idioto-odporne (np. sprawdzanie czy kolumna istnieje przed jej dodaniem), dobrym sposobem jest wersja tabeli w bazie danych (np. w osobnej tabeli)  

## Czasy życia obiektu: współdzielony/ulotny  
```cs
public class Foo{
    public static int Bar;
    public static int BarMethod(){
        int t = 0;
        return 42;
    }

    public int Qux;
    public int QuxMethod(){
        int t = 0;
        return 42;
    }
}
```

czym różni się pole statyczne od metody statycznej?  
```cs
// wywołanie
Foo.BarMethod();
(new Foo()).QuxMethod();
```

a co się dzieje z polem?
```cs
var _ = Foo.Bar;        // współdzielone
var __ = (new Foo()).Qux;   // ulotna, bezpieczniejsza
```

zmienne lokalne w metodzie współdzielonej nie są współdzielone  
bezpieczeństwo wywołania metody we współdzielonych wątkach sprawdzamy w dokumentacji w sekcji 'thread safety'  

## Trzeci czas życia w aplikacjach webowych: czas życia żądania
mamy 3 rodzaje kontenerów zarządzających czasem życia obiektów:
* `this.Application` static; interfejs .Add() oraz indexer (this.Application["item name"])
* `this.Session` jeden użytkownik na wszystkie żądania; interfejs analogiczne do Application
* `this.Items` od początku żądania do końca żądania

(kontenery te są niegeneryczne czyli zwracają Object i trzeba używać rzutowania)  

> obiekt wspólny dla wszystkich endpointów to `HttpContext context`, wtedy mamy dostęp do np `context.Items`   


## Jak się łączyć do bazy danych

> Jak dbać o dobry podział solution na projekty?  
> po pierwsze dbamy o to aby mieć odpowiedni fizyczny układ folderów  
> po drugie na poziomie solution możemy tworzyć wirtualne foldery na projekty  
> w projekcie aplikacji webowej byłyby wszystkie pliki odpowiedzialne za front-end  
> dodatkowe moduły np odpowiedzialne za jakąś logikę byłyby w osobnym projekcie

oddzielmy dostęp do bazy danych od aplikacji webowej  
```cs
public class CustomSqlConnectionSource {
    const string CONTEXTKEYNAME = "sqlconnectionkey";
    public SqlConnection GetConnection(HttpContext context){
        if(context.Items[CONTEXTKEYNAME] == null){
            var cs2 = ConfigurationManager.AppSettings["cs2"];
            var cs1 = ConfigurationManager.ConnectionStrings["cs1"].ConnectionString;

            var conn = new SqlConnection(cs1);
            context.Items.Add(CONTEXTKEYNAME, conn);
        }

        return (SqlConnection)context.Items[CONTEXTKEYNAME];
    }
}
```

> użytkownik główny aplikacji powinien dostać prawa właściciela (owner) bazy danych tej aplikacji   
w Web.config dodajemy connection string    
```xml
<!--! 2 metody -->
<configuration>
    <appSettings>
        <add key="cs2" value="server=.\sqlexpress;database=example;integrated security=true;trustservercertificate=true"/>
    </appSettings>

    <connectionStrings>
        <add name="cs1" connectionString="server=.\sqlexpress;database=example;integrated security=true;trustservercertificate=true"/>
    </connectionStrings>
</configuration>
```

## Jakiej technologii użyć do dostępu do bazy danych?
* Dapper (to teraz pokażemy)
* Entity Framework (lub EF Core)
  

teraz w naszej aplikacji webowej możemy korzystać z `CustomSqlConnectionSource`  
```cs
public void Page_Load(object sender, EventArgs e){
    var conn = new CustomSqlConnectionSource().GetConnection(HttpContext);

    var persons = conn.Query<Person>("select * from dbo.Person");

    var personList = string.Join(", ", persons.Select(p => p.Name));
}
```

* <%= %> surowe wypisywanie
* <%: %> enkodowane wypisywanie (zabezpiecza przez np sql injection)

## Gdzie jest koniec cyklu życia naszego połączenia z bazą danych?
### Global Application Class
mamy taki zasób jak `GlobalApplicationClass`, który ma funkcję `Application_End` lub `Application_EndRequest`    

### Abstract Base Page
możemy też skorzystać z zasobu `AbstractBasePage` którą wepchniemy wyżej w hierarchii naszych klas  
do naszej strony dodajemy interfejs IDisposable, który na koniec życia obiektu <=> trwania requesta wywoła się, i będziemy mogli wyrzucić obiekt


## Contener Dependency Injection
w .NET Core
```cs
var builder = WebApplication.CreateBuilder(args);
builder.Services
    // scoped <=> per żądanie
    .AddScoped<Foo>(services =>
    {
        return new Foo();
    })
    // lub
    .AddSingleton
    // lub
    .AddTransient


app.MapGet("/", (Foo foo) => "Hello World");

public class Foo {

}
```

nauczyliśmy teraz kontenera jak ma tworzyć obiekty Foo, jak z tego skorzystać do łączenia z bazą danych?

```cs
bulider.Services
    .AddScoped<SqlConnection>(service =>
    {
        IConfiguration cfg = service.GetRequiredService<IConfiguration>();
        var cs1 = cfg["cs1"];
        return new SqlConnection(cs1);
    });

app.MapGet("/", (SqlConnection conn) => {
    // tutaj potrzebny jest dapper
    var persons = conn.Query<Person>("select * from dbo.Person");
    return string.Join(", ", persons.Select(p => p.Name));
});
```

connection string możemy dodać w appsettings.json
```json
{
    "cs1": "server=.\sqlexpress;database=example;integrated security=true;trustservercertificate=true"
}
```

tutaj już nie musimy dodawać manualnie Dispose, bo kontener sam zadba o to przy końcu requesta



# Wykład 5 - MVC
widoki **muszą** być w folderze `Views`, natomiast modele i kontrolery mogą być w dowolnym miejscu (aczkolwiek wg konwencji w odpowiednich folderach)  



