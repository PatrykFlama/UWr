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
- [Wykład 6 - autentykacja i autoryzacja](#wykład-6---autentykacja-i-autoryzacja)
  - [Przykład autentykacji opartej o 302](#przykład-autentykacji-opartej-o-302)
    - [Wersja w .NET Framework, WebForms](#wersja-w-net-framework-webforms)
    - [Wersja w MVC](#wersja-w-mvc)
    - [Wersja w .NET Core](#wersja-w-net-core)



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



# Wykład 6 - autentykacja i autoryzacja
Typt autentykacji:  
* oparta o status HTTP 401 (odpowiedź typu "użytkownik musi się uwierzytelnić")  
* oparta o status HTTP 302  

można podmienić w tym statusie sposób autentykacji, np na opartą o protokół Cerberos - w samym Windowsie wbudowane są różne funkcje (typu daj token użytkownika, daj nazwę tego użytkownika)  

Wtedy w samym C# będziemy mieli `Request.User` z metodami dla już zalogowanego użytkownika. pytanie jak się ten użytkownik uwierzytelnia?

> dlaczego token JWT (doklejony do nagłówka zapytania) jest lepszy od ciastka?  
> bo ciastka nie są cross-domenowe, jeżeli zapytania będą wysyłane na różne serwery, to token z automatu będzie przenoszony do dowolnego serwera (np.: nasza jedna strona ma jakieś podaplikacje, chcemy aby użytkownik był uwierzytelniony we wszystkich, nie musząc się logować dla każdej z osobna)  


## Przykład autentykacji opartej o 302
### Wersja w .NET Framework, WebForms 

`this.User` przychodzi standarwo wraz z .NET

```cs
PageLoad {
    this.Label1.Text = $"username: {this.User.Identity.Name}, isauthenticated: {this.User.Identity.IsAuthenticated}";
}
```

Aby upewnić się, że użytkownik jest zalogowany, w MVC będziemy pisali atrybut. W webformsaach musimy w web.config dodać:
```xml
<system.web>
    <authetincation mode="Forms">
        <forms name="foo" loginUrl="/Login.aspx" />
    </authentication>
    <authorization>
        <deny users="?" />  <!-- odmawiaj niezalogowanym -->
        <allow users="*" /> <!-- zezwalaj zalogownym -->
    </authorization>
</system.web>
```

możliwe tryby: `Forms` - 302  
loginUrl to strona logowania, na którą będzie przekierowany użytkownik  

jeżeli chcemy inne reguły np dla admina to tworzymy dla niego osobny folder, np ForAdmin, i tam tworzymy kolejny web.config gdzie ustawiamy dla niego zsady autentykacji  

> co wykonuje przekierowania z web.config? jakiś wbudowany moduł odpowiedzialny za autentykację  


teraz możemy napisać stronę logowania, skorzystamy do niej z modułu `forms authentication`:  
```cs
PaleLoad() {

}

Button_Click() {
    if(hasło jest dobre) {
        var ticket = new FormAuthenticationTicket(TextBoxName.Text, false, 20);  // drugie pole to pytanie o utrwalenie ciastka - zapisanie go na jakiś czas (nie jest to to samo co ważność ciastka, jest to po stronie serwera)
        var cookieValue = FormsAuthentication.Encrypt(ticket);
        var cookie = new HttpCookie(FormsAuthentication.FormsCookieName, cookieValue);
        this.Response.AppendCookie(cookie);

        var redirectUrl = this.Request.QueyString["returnUrl"];
        this.Response.Redirect(redirectUrl);
    }
}

```


możemy dodać opcję slidingExpiration, która gdy minie połowa czasu ważności ciastka, przedłuża jego ważność (również po stronie serwera), wtedy tak długo jak użytkownik jest aktywny pozostanie on zalogowany, a po zamknięciu przeglądarki zostanie wylogowany  
```xml
<authentication mode>
  <forms name="foo" loginurl="" slidingExpiration="true" />
</authentication>
```

> Skąd wiedzieć kiedy pokazać użytkownikowi CAPTCHA? (bo chcemy tylko takiemu, który ma jakieś nieudane próby logowania)  
> robimy 2-krokowe logowanie - najpierw pokazujemy tylko login, potem pytamy serwer o ten login i on nam odpowiada czy wyświetlić mu CAPTCHA przy wpisywaniu hasła  

### Wersja w MVC
w notatkach powinna być

### Wersja w .NET Core
żeby wymusić autentykację używamy atrybutu `[Authorize]`  lub `[Autrize(schemat)]`  

w Main dodajemy do buildera autentykację (z notatek) (tutaj możemy tez np dodać własny cookie builder, albo cookie manager)  
w app dodajemy middleware (z notatek)  
reszta jest analogicznie jak wcześniej  