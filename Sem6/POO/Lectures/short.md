# Skrócona Notatka do Egzaminu

---

## Wykład 1: Unified Process

- **UP (Unified Process):** Iteracyjny proces wytwarzania oprogramowania, dzielący projekt na fazy i przyrosty.
    - **Inicjowanie:** Określenie celu projektu i wymagań na wysokim poziomie.
    - **Rozwinięcie:** Analiza i projektowanie architektury, identyfikacja ryzyk.
    - **Konstrukcja:** Implementacja i testowanie funkcjonalności.
    - **Przejście:** Wdrożenie systemu i przekazanie użytkownikom.

**Wymagania:**

- **FURPS+:** Klasyfikacja wymagań:
    - **Functional:** Funkcjonalność systemu (co robi).
    - **Usability:** Użyteczność, łatwość obsługi.
    - **Reliability:** Niezawodność, odporność na błędy.
    - **Performance:** Wydajność, szybkość działania.
    - **Supportability:** Łatwość utrzymania i rozwoju.
    - **+**: Dodatkowe aspekty (np. wymagania projektowe, implementacyjne).
- **S.M.A.R.T.:** Kryteria dobrze sformułowanych wymagań:
    - **Szczegółowe:** Jasno określone.
    - **Mierzalne:** Możliwe do zweryfikowania.
    - **Osiągalne:** Realistyczne do wykonania.
    - **Realistyczne:** Możliwe do osiągnięcia w danych warunkach.
    - **Terminowe:** Określone w czasie.
- **Przypadki Użycia:** Opisują interakcje aktora z systemem, pokazując scenariusze użycia (np. w języku Gherkin: Given-When-Then).

---

## Wykład 2: UML

**Diagramy:**

- **Strukturalne:**
    - **Diagram klas:** Przedstawia klasy, ich atrybuty, metody oraz relacje (np. dziedziczenie, asocjacje, agregacja – luźny związek, kompozycja – silny związek). Strategie mapowania dziedziczenia: **TPC** (Table per Concrete class), **TPH** (Table per Hierarchy), **TPT** (Table per Type).
    - **Diagram obiektów:** Pokazuje konkretne instancje klas i ich powiązania w danym momencie działania systemu.
- **Behawioralne:**
    - **Diagram stanów:** Opisuje możliwe stany obiektu i przejścia między nimi w odpowiedzi na zdarzenia.
    - **Diagram czynności:** Przedstawia przepływ wykonywanych kroków lub operacji (workflow, algorytm).
    - **Diagram sekwencji:** Pokazuje wymianę komunikatów między obiektami w czasie, ilustrując scenariusze działania.
    - **Diagram komponentów:** Przedstawia moduły systemu (komponenty) i ich zależności.
- **Diagrams-as-Code:** Narzędzia (np. **Mermaid.js**) umożliwiające tworzenie diagramów za pomocą kodu tekstowego, co ułatwia wersjonowanie i automatyzację.

---

## Wykład 3: SOLID & GRASP

- **GRASP:** Zbiór zasad projektowych wspierających dobre praktyki OOP:
    - **Information Expert:** Przekazuj odpowiedzialność klasie posiadającej najwięcej potrzebnych informacji.
    - **Low Coupling:** Minimalizuj powiązania między klasami, by ułatwić modyfikacje.
    - **High Cohesion:** Klasa powinna mieć spójny, jasno określony zakres odpowiedzialności.
    - **Polymorphism:** Wykorzystuj polimorfizm do obsługi różnych wariantów zachowań.
- **SOLID:** Pięć zasad projektowania obiektowego:
    - **SRP (Single Responsibility Principle):** Klasa powinna mieć tylko jedną odpowiedzialność.
    - **OCP (Open/Closed Principle):** Klasa powinna być otwarta na rozszerzenia, zamknięta na modyfikacje.
    - **LSP (Liskov Substitution Principle):** Obiekty podklas mogą być używane zamiast obiektów bazowych bez zmiany poprawności programu.
    - **ISP (Interface Segregation Principle):** Lepiej wiele małych, wyspecjalizowanych interfejsów niż jeden duży.
    - **DIP (Dependency Inversion Principle):** Zależności powinny być od abstrakcji, nie od konkretów.
- **Inne:**
    - **DRY (Don't Repeat Yourself):** Unikaj powielania kodu i logiki.
    - **Law of Demeter:** Klasy powinny komunikować się tylko z bezpośrednimi „sąsiadami” (ograniczanie zależności).


---
## Wykład 4: Wzorce Kreacyjne

- **Singleton:** Zapewnia, że klasa ma tylko jedną instancję i udostępnia do niej globalny punkt dostępu.
- **Fabryki:**
    - **Simple Factory:** Centralizuje logikę tworzenia obiektów, ukrywając szczegóły implementacji.
    - **Factory Method:** Pozwala podklasom decydować, jaki obiekt utworzyć, umożliwiając rozszerzalność.
    - **Abstract Factory:** Tworzy rodziny powiązanych obiektów bez określania ich konkretnych klas.
- **Builder:** Umożliwia stopniowe konstruowanie złożonych obiektów, oddzielając tworzenie od reprezentacji.
- **Prototype:** Tworzy nowe obiekty przez klonowanie istniejących instancji.
- **Object Pool:** Zarządza pulą gotowych do użycia obiektów, ograniczając koszt ich tworzenia.

---

## Wykład 5: Wzorce Strukturalne

- **Fasada:** Udostępnia uproszczony interfejs do złożonego systemu lub zestawu klas.
- **Dekorator:** Pozwala dynamicznie dodawać nowe funkcjonalności do obiektów bez zmiany ich kodu.
- **Proxy:** Podstawia obiekt pośredniczący, który kontroluje dostęp do innego obiektu (np. opóźnione ładowanie, zabezpieczenia).
- **Adapter:** Przekształca interfejs jednej klasy na inny oczekiwany przez klienta.
- **Pyłek (Flyweight):** Optymalizuje zużycie pamięci przez współdzielenie wspólnego stanu wielu obiektów.

---

## Wykład 6: Wzorce Czynnościowe 1

- **Null Object:** Zamiast wartości null stosuje się obiekt wykonujący „nic”, co upraszcza obsługę wyjątków.
- **Iterator:** Umożliwia sekwencyjne przechodzenie po elementach kolekcji bez ujawniania jej implementacji.
- **Composite:** Pozwala traktować pojedyncze obiekty i ich złożenia w jednolity sposób (struktury drzewiaste).
- **Visitor:** Umożliwia dodawanie nowych operacji do struktur obiektów bez ich modyfikacji.

---

## Wykład 7: Wzorce Czynnościowe 2

- **Mediator:** Centralizuje komunikację między obiektami, redukując ich bezpośrednie powiązania.
- **Observer:** Pozwala obiektom powiadamiać inne o zmianach swojego stanu (mechanizm subskrypcji).
- **Event Aggregator:** Konsoliduje rozsyłanie i odbieranie zdarzeń w systemie.
- **Memento:** Pozwala zapisywać i przywracać stan obiektu bez naruszania jego hermetyzacji.

---

## Wykład 8: Wzorce Czynnościowe 3

- **Chain of Responsibility:** Przekazuje żądanie przez łańcuch obiektów, aż któryś je obsłuży.
- **Command:** Enkapsuluje żądanie jako obiekt, umożliwiając np. cofanie operacji.
- **Template Method:** Definiuje szkielet algorytmu w klasie bazowej, pozostawiając szczegóły podklasom.
- **Strategy:** Pozwala wybierać algorytm w czasie działania przez kompozycję zamiast dziedziczenia.

---

## Wykład 10: IoC / DI

- **IoC (Inversion of Control):** Przekazanie kontroli nad tworzeniem i powiązaniami obiektów z aplikacji do zewnętrznego mechanizmu.
- **DI (Dependency Injection):** Wstrzykiwanie zależności do obiektów zamiast tworzenia ich wewnątrz klasy.
- **Zależności twarde:** Bezpośrednie powiązania z konkretnymi klasami.
- **Zależności miękkie:** Odwołania do abstrakcji (np. interfejsów), ułatwiające testowanie i modyfikacje.
- **Cykl życia:** Określa, jak długo obiekt istnieje (np. krótkotrwały, singleton, na wątek).
- **Kontener DI:** Narzędzie zarządzające tworzeniem i dostarczaniem zależności.

---

## Wykład 11: Repository & UoW

- **Repository:** Udostępnia zunifikowany interfejs do operacji na danych, ukrywając szczegóły dostępu do źródła danych.
- **Unit of Work (UoW):** Koordynuje zmiany w wielu repozytoriach i zarządza transakcjami jako jedną całością.
- **Abstrakcje modeli:** Definiowanie interfejsów dla warstwy dostępu do danych.
- **Local Factory:** Dynamiczne tworzenie instancji UoW lub repozytoriów.
- **Ostrzeżenie:** Nie należy zwracać `IQueryable` z repozytorium, by nie ujawniać szczegółów implementacji.

---

## Wykład 12: Architektura Aplikacji

- **MVC (Model-View-Controller):** Rozdziela logikę biznesową, prezentację i obsługę wejścia użytkownika (głównie web).
- **MVP (Model-View-Presenter):** Widok przekazuje logikę do Presentera (często w aplikacjach desktopowych).
- **MVVM (Model-View-ViewModel):** Umożliwia dwukierunkowe wiązanie danych między widokiem a modelem (np. WPF).
- **Architektura Heksagonalna:** Oddziela logikę biznesową od interfejsów zewnętrznych przez porty i adaptery.
- **Kryteria:** Testowalność, łatwość rozwoju i skalowania, przejrzystość odpowiedzialności.

---

## Kluczowe Wnioski

- **SOLID/GRASP:** Fundamentalne zasady projektowania obiektowego, poprawiające jakość kodu.
- **Wzorce:** Kreacyjne (tworzenie obiektów), strukturalne (organizacja klas/obiektów), czynnościowe (zarządzanie zachowaniem).
- **Architektura:** Warstwowa, heksagonalna – izolacja odpowiedzialności, łatwość testowania.
- **Narzędzia:** DI, Repository/UoW – wspierają czystą architekturę i elastyczność kodu.

