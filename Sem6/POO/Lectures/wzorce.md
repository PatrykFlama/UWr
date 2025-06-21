# Wzorce projektowe obiektowe z dokumentacji

## 1. Singleton
**Cel:** Zapewnienie istnienia tylko jednej instancji klasy w całym systemie  
**Działanie:**
- Prywatny konstruktor blokujący bezpośrednie tworzenie
- Statyczna właściwość/metoda zwracająca istniejącą instancję
- Leniwa inicjalizacja (tworzenie przy pierwszym dostępie)  
**Zastosowanie:** Logowanie, połączenia do bazy danych, cache

**Przykład:**
```csharp
class Logger {
    private static Logger instance;
    private Logger() {}
    public static Logger Instance => instance ??= new Logger();
}
```

---

## 2. Monostate
**Cel:** Wiele instancji współdzielących ten sam stan  
**Działanie:**
- Wszystkie pola statyczne
- Każda instancja operuje na wspólnych danych  
**Różnica od Singleton:** Możliwość tworzenia wielu obiektów, ale ze współdzielonym stanem

**Przykład:**
```csharp
class Settings {
    private static int value;
    public int Value { get => value; set => Settings.value = value; }
}
```

---

## 3. Fabryka (Delegate Factory)
**Cel:** Enkapsulacja logiki tworzenia obiektów  
**Działanie:**
- Wydzielona klasa/metoda odpowiedzialna za tworzenie obiektów
- Zwraca interfejsy zamiast konkretnych implementacji  
**Zalety:** Zgodność z OCP, łatwa wymiana implementacji

**Przykład:**
```csharp
interface IAnimal { }
class Dog : IAnimal { }
class AnimalFactory {
    public IAnimal CreateDog() => new Dog();
}
```

---

## 4. Factory Method
**Cel:** Delegowanie tworzenia obiektów do podklas  
**Działanie:**
- Klasa bazowa deklaruje abstrakcyjną metodę fabrykującą
- Podklasy implementują konkretne tworzenie obiektów  
**Wzorzec:** Creator → FactoryMethod() → Product

**Przykład:**
```csharp
abstract class Dialog {
    public abstract IButton CreateButton();
}
class WindowsDialog : Dialog {
    public override IButton CreateButton() => new WindowsButton();
}
```

---

## 5. Abstract Factory
**Cel:** Tworzenie rodzin powiązanych obiektów  
**Działanie:**
- Interfejs z metodami do tworzenia różnych produktów
- Konkretne fabryki implementują tworzenie spójnych rodzin  
**Przykład:** Fabryka elementów GUI dla różnych systemów (Windows/Mac)

**Przykład:**
```csharp
interface IGuiFactory { IButton CreateButton(); ICheckbox CreateCheckbox(); }
class WinFactory : IGuiFactory { ... }
class MacFactory : IGuiFactory { ... }
```

---

## 6. Prototype
**Cel:** Tworzenie obiektów przez klonowanie  
**Działanie:**
- Implementacja metody `Clone()`
- Płytka/głęboka kopia w zależności od potrzeb  
**Zastosowanie:** Gdy tworzenie obiektu jest kosztowne

**Przykład:**
```csharp
class Shape {
    public Shape Clone() => (Shape)this.MemberwiseClone();
}
```

---

## 7. Object Pool
**Cel:** Reużywanie kosztownych obiektów  
**Działanie:**
- Pula przechowuje inicjalizowane obiekty
- Metoda `Acquire()` - zwraca obiekt z puli
- Metoda `Release()` - zwraca obiekt do puli  
**Zastosowanie:** Połączenia bazodanowe, wątki

**Przykład:**
```csharp
class Pool {
    private Queue<Connection> pool = new();
    public Connection Acquire() => pool.Dequeue();
    public void Release(Connection c) => pool.Enqueue(c);
}
```

---

## 8. Builder
**Cel:** Konstrukcja złożonych obiektów krok po kroku  
**Elementy:**
- **Builder:** Interfejs z metodami budowania części
- **Director:** Koordynuje proces budowy
- **Product:** Wynikowy obiekt  
**Zaleta:** Możliwość tworzenia różnych reprezentacji tego samego produktu

**Przykład:**
```csharp
class BurgerBuilder {
    public BurgerBuilder AddCheese() { ... return this; }
    public Burger Build() => new Burger();
}
```

---

## 9. Fasada (Facade)
**Cel:** Uproszczony interfejs dla złożonego systemu  
**Działanie:** Klasa ukrywająca złożoność podsystemu  
**Przykład:** Uproszczone API dla skomplikowanej biblioteki

**Przykład:**
```csharp
class ComputerFacade {
    public void Start() { cpu.Boot(); disk.Load(); }
}
```

---

## 10. Pyłek (Flyweight)
**Cel:** Efektywne zarządzanie wieloma drobnymi obiektami  
**Klucz:**
- Stan wewnętrzny (współdzielony)
- Stan zewnętrzny (unikalny)  
**Zastosowanie:** Systemy czcionek, gry (np. drzewa w lesie)

**Przykład:**
```csharp
class TreeType { /* współdzielony stan */ }
class Tree { TreeType type; int x, y; }
```

---

## 11. Dekorator (Decorator)
**Cel:** Dynamiczne dodawanie funkcjonalności  
**Działanie:**
- Opakowuje oryginalny obiekt
- Implementuje ten sam interfejs
- Deleguje wywołania + dodaje własne funkcje  
**Przykład:** Dodatki do napojów (cukier, mleko)

**Przykład:**
```csharp
class SugarDecorator : Coffee {
    private Coffee coffee;
    public override double Cost() => coffee.Cost() + 0.5;
}
```

---

## 12. Pełnomocnik (Proxy)
**Typy:**
- Wirtualny: Leniwe tworzenie
- Ochronny: Kontrola dostępu
- Zdalny: Komunikacja sieciowa
- Logujący: Rejestracja dostępu
- Circuit Breaker: Specjalny proxy do obsługi błędów

**Przykład:**
```csharp
class ImageProxy : IImage {
    private RealImage realImage;
    public void Display() {
        if (realImage == null) realImage = new RealImage();
        realImage.Display();
    }
}
```

---

## 13. Adapter
**Cel:** Uzgadnianie niezgodnych interfejsów  
**Struktura:** Klient → Target ← Adapter ← Adaptee  
**Zastosowanie:** Integracja starszych komponentów

**Przykład:**
```csharp
class OldPrinter { public void PrintOld(string s) { ... } }
class PrinterAdapter : IPrinter {
    private OldPrinter oldPrinter;
    public void Print(string s) => oldPrinter.PrintOld(s);
}
```

---

## 14. Most (Bridge)
**Cel:** Oddzielenie abstrakcji od implementacji  
**Działanie:** Dwie niezależne hierarchie klas  
**Zaleta:** Niezależne rozwijanie obu hierarchii

**Przykład:**
```csharp
interface IDraw { void DrawCircle(); }
class RedDraw : IDraw { ... }
class Circle {
    private IDraw draw;
    public void Draw() => draw.DrawCircle();
}
```

---

## 15. Null Object
**Cel:** Bezpieczne zastępstwo dla wartości null  
**Działanie:** Obiekt z pustymi implementacjami metod  
**Zaleta:** Eliminuje sprawdzanie `if (obj != null)`

**Przykład:**
```csharp
class NullLogger : ILogger {
    public void Log(string msg) { /* nic nie robi */ }
}
```

---

## 16. Iterator
**Cel:** Sekwencyjny dostęp do elementów kolekcji  
**Elementy:**
- **Iterator:** Interfejs z `getNext()`, `hasNext()`
- **Aggregate:** Interfejs do tworzenia iteratora

**Przykład:**
```csharp
class MyIterator {
    private int[] data; int pos = 0;
    public bool HasNext() => pos < data.Length;
    public int Next() => data[pos++];
}
```

---

## 17. Composite
**Cel:** Hierarchia drzewiasta obiektów  
**Struktura:**
- **Component:** Interfejs wspólny
- **Leaf:** Element końcowy
- **Composite:** Element z dziećmi

**Przykład:**
```csharp
abstract class Graphic { public abstract void Draw(); }
class Line : Graphic { ... }
class Group : Graphic {
    List<Graphic> children;
    public override void Draw() { foreach(var c in children) c.Draw(); }
}
```

---

## 18. Visitor
**Cel:** Dodawanie operacji bez modyfikacji klas  
**Działanie:**
- Obiekt Visitor z metodami dla każdego typu
- Elementy akceptują visitora (`Accept()`)
- **Double Dispatch:** Wybór metody na podstawie typu elementu i visitora

**Przykład:**
```csharp
interface IVisitor { void VisitCircle(Circle c); }
class Circle { public void Accept(IVisitor v) => v.VisitCircle(this); }
```

---

## 19. Mediator
**Cel:** Centralizacja komunikacji między obiektami  
**Działanie:** Obiekty komunikują się tylko przez mediatora  
**Różnica od Observer:** Ograniczona, znana grupa uczestników

**Przykład:**
```csharp
class ChatMediator {
    public void Send(string msg, User user) { /* rozsyła wiadomość */ }
}
```

---

## 20. Observer
**Cel:** Powiadamianie o zmianach stanu  
**Elementy:**
- **Subject:** Obserwowany obiekt
- **Observer:** Interfejs obserwatorów  
**Implementacja:** Lista subskrybentów

**Przykład:**
```csharp
class Subject {
    List<IObserver> observers;
    public void Notify() { foreach(var o in observers) o.Update(); }
}
```

---

## 21. Event Aggregator
**Cel:** Uogólniony mechanizm subskrypcji  
**Działanie:** Centralny broker zdarzeń  
**Zaleta:** Luźniejsze powiązania niż Observer

**Przykład:**
```csharp
class EventAggregator {
    public void Subscribe<T>(Action<T> handler) { ... }
    public void Publish<T>(T evt) { ... }
}
```

---

## 22. Memento
**Cel:** Zapamiętywanie i przywracanie stanu  
**Elementy:**
- **Originator:** Tworzy/przywraca stan
- **Memento:** Przechowuje stan
- **Caretaker:** Zarządza historią  
**Rozszerzenie:** Mechanizm Undo/Redo

**Przykład:**
```csharp
class Memento { public string State; }
class Originator {
    public Memento Save() => new Memento { State = this.State };
    public void Restore(Memento m) { this.State = m.State; }
}
```

---

## 23. Chain of Responsibility
**Cel:** Przekazywanie żądań w łańcuchu  
**Działanie:** Każdy handler decyduje o przetworzeniu lub przekazaniu  
**Zastosowanie:** Pipeline przetwarzania żądań

**Przykład:**
```csharp
abstract class Handler {
    protected Handler next;
    public void SetNext(Handler n) => next = n;
    public abstract void Handle(Request r);
}
```

---

## 24. Command
**Cel:** Enkapsulacja żądań jako obiektów  
**Elementy:**
- **Command:** Interfejs z `Execute()`
- **Receiver:** Wykonawca logiki  
**Zastosowanie:** Kolejki poleceń, operacje Undo

**Przykład:**
```csharp
interface ICommand { void Execute(); }
class LightOnCommand : ICommand { public void Execute() { light.On(); } }
```

---

## 25. Template Method
**Cel:** Szablon algorytmu z wariantowymi krokami  
**Działanie:**
- Metoda szkieletowa w klasie bazowej
- Abstrakcyjne/chronione metody do implementacji  
**Zaleta:** Reużycie struktury algorytmu

**Przykład:**
```csharp
abstract class Game {
    public void Play() { Start(); PlayTurn(); End(); }
    protected abstract void PlayTurn();
}
```

---

## 26. Repository
**Cel:** Abstrakcja dostępu do danych  
**Typy:**
- Generic Repository (`IRepository<T>`)
- Concrete Repository (specjalizowane)  
**Zaleta:** Izolacja logiki biznesowej od danych

**Przykład:**
```csharp
interface IRepository<T> { void Add(T item); T Get(int id); }
class UserRepository : IRepository<User> { ... }
```

---

## 27. Unit of Work
**Cel:** Zarządzanie transakcjami i kontekstem  
**Działanie:**
- Koordynacja wielu repozytoriów
- Zarządzanie transakcjami
- Śledzenie zmian obiektów

**Przykład:**
```csharp
class UnitOfWork {
    public void Commit() { /* zapisuje zmiany w repozytoriach */ }
}
```

---

## 28. Dependency Injection
**Cel:** Odwrócenie kontroli tworzenia zależności  
**Sposoby wstrzykiwania:**
- Przez konstruktor
- Przez właściwości
- Przez metody  
**Zalety:** Testowalność, wymienialność modułów

**Przykład:**
```csharp
class Service {
    private IRepository repo;
    public Service(IRepository repo) { this.repo = repo; }
}
```

---

## 29. Inversion of Control
**Zasada:** Moduły wyższego poziomu nie zależą od implementacji  
**Realizacja:** Przez Dependency Injection  
**Composition Root:** Miejsce konfiguracji kontenera DI

**Przykład:**
```csharp
// Konfiguracja zależności w jednym miejscu
container.Register<IRepository, UserRepository>();
```

---

## 30. Model-View-Controller (MVC)
**Elementy:**
- **Model:** Dane i logika biznesowa
- **View:** Prezentacja
- **Controller:** Przyjmuje żądania, koordynuje  
**Cykl:** Użytkownik → Controller → Model → View

**Przykład:**
```csharp
class Controller {
    public void OnClick() { model.Update(); view.Render(); }
}
```

---

## 31. Model-View-Presenter (MVP)
**Elementy:**
- **Model:** Dane
- **View:** Interfejs + delegacja do Presentera
- **Presenter:** Logika prezentacji  
**Różnica od MVC:** View jest pasywne, deleguje do Presentera

**Przykład:**
```csharp
class Presenter {
    public void OnButton() { model.Do(); view.Show(); }
}
```

---

## 32. Model-View-ViewModel (MVVM)
**Elementy:**
- **Model:** Dane
- **View:** Interfejs użytkownika
- **ViewModel:** Pomost z logiką prezentacji  
**Mechanizm:** Data binding między View a ViewModel

**Przykład:**
```csharp
class ViewModel {
    public string Name { get; set; }
}
```

---

## 33. Architektura Heksagonalna
**Cel:** Izolacja rdzenia biznesowego  
**Elementy:**
- **Porty:** Interfejsy wejścia/wyjścia
- **Adaptery:** Implementacje portów  
**Zasada:** Rdzeń nie zna szczegółów implementacyjnych

**Przykład:**
```csharp
interface IPort { void Save(); }
class FileAdapter : IPort { public void Save() { ... } }
```
