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
- [Wykład 8](#wykład-8)
  - [FIDO2 dla .NET Framework](#fido2-dla-net-framework)
- [Wykład 9 - WebAPI i REST](#wykład-9---webapi-i-rest)
  - [REST vs SOAP](#rest-vs-soap)
  - [.NET Framewrok](#net-framewrok)
    - [GET](#get)
    - [POST](#post)
    - [Jak zakazać GET](#jak-zakazać-get)
  - [.NET Core](#net-core)
  - [API key](#api-key)
  - [Tokeny JWT](#tokeny-jwt)
- [Wykład 10 - SOAP](#wykład-10---soap)
  - [ASP.NET Framework](#aspnet-framework)
    - [WSDL](#wsdl)
    - [WCF](#wcf)
  - [ASP.NET Core](#aspnet-core)
- [Wykład 11 - SignalR oraz MediatR](#wykład-11---signalr-oraz-mediatr)
- [Wykład 12 - ClickOnce, gRPC](#wykład-12---clickonce-grpc)
  - [gRPC](#grpc)
  - [ClickOnce](#clickonce)



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

# Wykład 8
## FIDO2 dla .NET Framework
https://github.com/wzychla/Fido2.NetFramework

# Wykład 9 - WebAPI i REST
## REST vs SOAP  
REST jest bardziej dedykowany dla komunikacji serwer->przeglądarka, natomiast SOAP jest bardziej dedykowany dla komunikacji serwer->serwer  

## .NET Framewrok
tworząc nowy projekt wybieramy WebAPI _oraz_ MVC (możemy skorzystac z obu)  
w folderze controllers zrobimy osobne foldery na MVC oraz WebAPI, żeby nam się nie pomieszały  
`./App_Start/RouterConfig.cs` - zawiera konfigurację routigu dla MVC  
`./App_Start/WebApiConfig.cs` - zawiera konfigurację routingu dla WebAPI  
dlatego że MVC ma być ścieżką domyślną to zarejestrujemy ją po WebAPI  
żeby ścieżki idące do kontrolerów WebAPI były rozróżniane od tych idących do kontrolerów MVC, dodajemy prefix `api` do ścieżki: `api/{controller}/{id}`  
> dlaczego w WebAPI nie ma w ścieżce {action}?  
> bo w WebAPI mamy tylko jedną akcję, dla GET/POST/PUT/DELETE

tworząc kontroler do WebAPI musimy wybrać (w trakcie tworzenia) odpowiedni szablon! (w nim klasa kontrolera dziedziczy po ApiController)  
z nazwy funkcji kontrolera w weapi wynika jaką akcję obsługuje  

### GET
```cs
public class PersonController : ApiController
{
    // oba zapytania działają, bo są rozróżnione po ścieżce
    public Person Get()
    {
        return new Person { Name = "Jan" };
    }

    public Person Get(string id)
    {
        return new Person { Name = "Jan" + id };
    }
}

public class Person
{
    public string Name { get; set; }
}
```

domyślnie zostanie zwrócony XML, aby to zmienić dodajemy w WebApiCongig.cs:
```cs
config.Formatters.Remove(GlobalConfiguration.Configuration.Formatters.XmlFormatter);
```

> zasada na oko: do 3 parametrów w URL zapytania GET mają jeszcze sens

### POST
```cs
public PersonPostResponseModel Post(PersonPostRequestModel model)
{
    return new PersonPostResponseModel();
}

public class PersonPostRequestModel {}
public class PersonPostResponseModel {}
```

### Jak zakazać GET
```cs
public IHttpActionResult Get(string id)
{
    // tutaj np możemy przeproawdzić weryfikację zapytania
    if(id == "foo") {
        return this.BadRequest();
    }

    return this.Ok(
        new Person({ Name = "Jan" });
    );
}
```

## .NET Core
Tworzymy po prostu nowy pusty projekt (ewentualnie skorzystamy z szablonu dla MVC - jest już tam skonfigurowany routing)  

W core kontrolery dla webapi i dla mvc to te same routy   
tworzymy z dedykowanego szblonu dla webapi - dziedziczy on po `ControllerBase` oraz ma nadpisaną ścieżkę `[Route("api"/[controller])]`  
działa to praktycznie tak samo jak w .NET Framework
```cs
public IActionResult Get()
{
    return this.Ok(new Person(){} );
}
```

w .NET Core jak ustawimy funkcji w kontrolerze atrybut `[HttpGet]` to nasza funkcja może mieć już dowolną nazwę  

## API key
przy komunikacji serwer-serwer nie mamy dostępu do ciasteczek = autentykacji, więc potrzebujemy innego sposobu na autentykację - kluczy z dostatecznie dużą entropią  
do tego możemy stworzyć własny filtr do autentykacji za pomocą api key, wtedy zamiast `[Authorize]` nad funkcją będziemy pisać `[CustomAuthenticationFilter]`  
w standardzie mamy już dedykowany nagłówek `Authorization` który ma albo wartość `Basic` która trzymma proste klucze (np klucz api), albo coś bardziej skomplikowanego   

## Tokeny JWT
Sensowna alternatywa dla prymitywnych uwierzytelnień stałym kluczem (mogą się często zmieniać, przez co wykradnięcie ich nie tworzy dużego ryzyka)  
niestety dla nich w .NET Framework musimy sami napisać filtry autentykacyjne  
w .NET Core do autentykacji służy funkcja `.JWTAddBearer`, która pozwala nam zweryfikować token jwt  

wtedy w nagłówku HTML mamy `"Authorization": Bearer ${token}`, gdzie bearer jest dedykowany dla 'wygasających' kluczy  

walidacja symetryczna vs asymetryczna: 
* symetryczna enkoduje i dekoduje tylko jednym kluczem, więc nie możemy się nim z nikim podzielić (więc tylko nasz serwer powinien wydawać i autentykować klucze)
* asymetryczna ma klucz prywatny do szyfrowania i publiczny do weryfikowania, więc możemy wydać klucz jednym serwerem i zautentykować go innym 

# Wykład 10 - SOAP
## ASP.NET Framework
### WSDL
to plik opisujący nasz serwis (pratkycznie zawsze jest on generowany automatycznie, nikt tego nie robi manualnie)  
można go wygenerować jakimś narzędziem, i na podstawie takiego pliku stworzyć serwer oraz klienta, albo można też stworzyć serwer, z niego wyprodukować plik WSDL, a na podstawie tego pliku stworzyć klienta (co na tym wykładzie przećwiczymy)  

_____

tworzymy kompletnie pusty asp.net framework web app  

> aktualnie w asp.net core też można już za pomocą WCF konstruować serwisy SOAP

dodajemy web service ASMX  

tutaj nie mamy części klienckiej, tylko jest zapytanie o wynik oraz jego zwrócenie - to już będzie nasza usługa sieciowa  
po uruchomieniu serwera i przejściu na link, (paradoksalnie) pojawia się jakaś strona - jest to strona z opisem naszej usługi sieciowej (zawiera ona link do pliku WSDL przechodząc z parametrem zapytania `?wsdl`)   

_____

przykład:  
tworzymy modele
```cs
public class WebService1RequestModel
{
    public string Name { get; set; }
    public int Age { get; set; }
}

public class WebService1ResponseModel
{
    public string Name { get; set; }
    public int Age { get; set; }
}
```

następnie dodajemy metodę do naszego serwisu
```cs
[WebMethod]
public WebService1ResponseModel WebService1(WebService1RequestModel model)
{
    return new WebService1ResponseModel
    {
        Name = model.Name,
        Age = model.Age
    };
}
```

stwórzmy nowy projekt webforms (dzięki temu że jest on w tym samym solution to będzie się on z naszym serwerem po localhost komunikować)  
znajdujemy `wsdl.exe` w sdk dla .NET Framework, i generujemy na jego podstawie plik `WebService1.cs`  
```bash
wsdl.exe http://localhost:1234/WebService1.asmx?wsdl
```

ewentualnie dzięki visual studio możemy zrobić to samo, klikając prawym na projekt i wybierając `Add Service Reference`; wtedy możemy wybrać naszą usługę i stworzyć serwis sieciowy  
utworzy się wtedy klasa proxy, która pozwala nam na komunikację z serwerem  

podepnijmy teraz pod przycisk w webforms naszą akcję
```cs
protected void Button1_Click(object sender, EventArgs e)
{
    WebService1SoapClient client = new WebService1SoapClient();
    
    var response = client.WebService1(new WebService1RequestModel
    {
        Name = "Jan",
        Age = 42
    });

    MessageBox.Show(response.Name + " " + response.Age);
}
```

### WCF
tworzenie analogiczne, ale wybieramy `WCF Service`  
w kodzie mamy `IService1.cs` oraz `Service1.svc`

zaimplementujmy poprzedni przykład:  
`IService1.cs`
```cs
[ServiceContract]
public interface IService1
{
    [OperationContract]
    WebService1ResponseModel WebService1(WebService1RequestModel model);
}
```

`Service1.svc.cs`
```cs
public class Service1 : IService1
{
    public WebService1ResponseModel WebService1(WebService1RequestModel model)
    {
        return new WebService1ResponseModel
        {
            Name = model.Name,
            Age = model.Age
        };
    }
}
```

teraz możemy dodać klienta, ale został on przyspieszony, żeby z tego skorzystać musimy kliknąć w advanced, gdzie odpali się nowe okno do dodawania klientów nowego typu  


## ASP.NET Core
> przykład z wykładu - jak to akutlanie się robi  



# Wykład 11 - SignalR oraz MediatR
hands on z wykładu - gdzie zaimplementować logikę obsługi maili?  
tworzymy nowy projekt `PortsImpl` w którym tworzymy klasę `EmailSender` dziedziczącą co IEmailSender z metodą `SendEmail`  

teraz możemy w starcie naszego programu (w `Program.cs`) zarejestrować nasz serwis w kontenerze DI  
```cs
bulider.Services.AddScoped<IEmailSender, EmailSender>();
```

takie miejsce, na konfigurację aplikacji naszymi bibliotekami, jest nazywane CompositionRoot i możemy je wyekstraktować do osobnej klasy `CompositionRoot`  
```cs
private static void CompositionRoot(WebApplicationBuilder builder)
{
    builder.Services.AddScoped<IEmailSender, EmailSender>();
}
```

___
teraz możemy łatwo napisać testy jednostkowe  
skorzystamy z biblioteki mock do wytwarania typów zastępczych (w runtime generuje typów które implementują dany interfejs)  

```cs 
namespace UnitTests.LogonUseCaseTestsSpace
{
    [TestClass]
    public class LogonUseCaseTests
    {
        [TestMethod]
        public async Task SucessScenario()
        {
            var emailPortMock = new Mock<IEmailSender>();
            var useUseCase = new LogonUseCase(emailPortMock.Object);
            var result = await useUseCase.Handle(new LogonUseCaseRequestModel
            {
                Password = "foo",
                Username = "bar"
            }, new CancellationToken());

            Assert.IsTrue(result.Success);
            Assert.AreEqual("bar", result.Username);

            emailPortMock.Verify(x => x.SendEmail(It.IsAny<string>(), It.IsAny<string>()), Times.Once);
        }
    }
}
```

# Wykład 12 - ClickOnce, gRPC
## gRPC
tldr: niby tylko ciekawostka ale potencjalenie użyteczne gdy chcemy zwiększyć przepustowość komunikacji (lepiej skompresowane dane)  
przykłady z wykładu

## ClickOnce
>> zasobnik cerytifkatów w windows to MMC (Microsoft Management Console)  

tldr; narzędzie do tworzenia instalatorów, które pozwala na proste zautomatyzowane zainstalowanie aplikacji za pomocą linku w przeglądarce (cons: działa tylko na windows i na ms edge)