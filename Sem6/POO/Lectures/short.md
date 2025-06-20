# Skrócona Notatka do Egzaminu

---

## Wykład 1: Unified Process

- **UP:** Iteracyjny proces z fazami:
    - Inicjowanie
    - Rozwinięcie
    - Konstrukcja
    - Przejście

**Wymagania:**

- **FURPS+:** Functional, Usability, Reliability, Performance, Supportability (+Design, Implementation)
- **S.M.A.R.T.:** Szczegółowe, Mierzalne, Osiągalne, Realistyczne, Terminowe
- **Przypadki Użycia:** Opis interakcji aktor-system (np. w Gherkin)

---

## Wykład 2: UML

**Diagramy:**

- **Strukturalne:**
    - Diagram klas (agregacja vs. kompozycja, strategie mapowania dziedziczenia: TPC/TPH/TPT)
    - Diagram obiektów
- **Behawioralne:**
    - Diagram stanów
    - Diagram czynności (przepływ kroków)
    - Diagram sekwencji (interakcje w czasie)
    - Diagram komponentów

- **Diagrams-as-Code:** Narzędzia jak Mermaid.js

---

## Wykład 3: SOLID & GRASP

- **GRASP:** Information Expert, Low Coupling, High Cohesion, Polymorphism
- **SOLID:**
    - SRP (jedna odpowiedzialność)
    - OCP (otwarte/rozszerzalne)
    - LSP (zastępowalność)
    - ISP (małe interfejsy)
    - DIP (zależności od abstrakcji)
- **Inne:** DRY, Law of Demeter

---

## Wykład 4: Wzorce Kreacyjne

- **Singleton:** Jedna instancja globalna
- **Fabryki:**
    - Simple Factory (enkapsulacja tworzenia)
    - Factory Method (tworzenie w podklasach)
    - Abstract Factory (rodziny powiązanych obiektów)
- **Builder:** Konstrukcja złożonych obiektów krok po kroku
- **Inne:** Prototype (klonowanie), Object Pool (reużywanie obiektów)

---

## Wykład 5: Wzorce Strukturalne

- **Fasada:** Uproszczony interfejs do złożonego systemu
- **Dekorator:** Dynamiczne rozszerzanie funkcjonalności (np. SugarDecorator)
- **Proxy:** Wirtualny (lazy loading), Ochronny, Circuit Breaker
- **Adapter:** Uzgodnienie niezgodnych interfejsów
- **Pyłek (Flyweight):** Współdzielenie stanu (np. bierki w szachach)

---

## Wykład 6: Wzorce Czynnościowe 1

- **Null Object:** Bezpieczny obiekt zamiast null
- **Iterator:** Sekwencyjny dostęp do kolekcji
- **Composite:** Hierarchie drzewiaste (Component, Leaf, Composite)
- **Visitor:** Dodawanie operacji bez modyfikacji klas (Double Dispatch)

---

## Wykład 7: Wzorce Czynnościowe 2

- **Mediator:** Centralna komunikacja w grupie obiektów
- **Observer:** Powiadamianie o zmianach (np. zdarzenia w C#)
- **Event Aggregator:** Uogólniony mechanizm powiadomień
- **Memento:** Zapisywanie/przywracanie stanu (Undo/Redo)

---

## Wykład 8: Wzorce Czynnościowe 3

- **Chain of Responsibility:** Łańcuch handlerów (każdy decyduje o przetwarzaniu)
- **Command:** Enkapsulacja żądań jako obiektów (np. do Undo)
- **Template Method:** Szkielet algorytmu w klasie bazowej
- **Strategy:** Wymienne algorytmy (kompozycja zamiast dziedziczenia)

---

## Wykład 10: IoC / DI

- **IoC vs DI:** Inwersja kontroli (zasada) vs. Wstrzykiwanie zależności (implementacja)
- **Zależności:** Twarde (stabilne) vs. miękkie (wymagające abstrakcji)
- **Cykl życia:** Transient, Singleton, PerThread
- **Kontenery:** Rejestracja typów, zarządzanie zależnościami (np. Unity)

---

## Wykład 11: Repository & UoW

- **Repository:** Abstrakcja dostępu do danych (np. `IGenericRepository<T>`)
- **Unit of Work (UoW):** Koordynacja transakcji i wielu repozytoriów

**Architektura:**

- Abstrakcje modeli (interfejsy)
- Local Factory (dynamiczne tworzenie UoW)

**Ostrzeżenia:** Unikać zwracania `IQueryable` z repozytorium

---

## Wykład 12: Architektura Aplikacji

**Wzorce UI:**

- **MVC (web):** Model-Widok-Kontroler
- **MVP (desktop):** Widok deleguje logikę do Presentera
- **MVVM (WPF/Xamarin):** Data binding z ViewModel

**Architektura Heksagonalna:**

- Porty (pierwotne/wtórne) i adaptery
- Izolacja rdzenia biznesowego

**Kryteria:** Testowalność, skalowalność, utrzymywalność

---

## Kluczowe Wnioski

- **SOLID/GRASP:** Podstawa projektowania obiektowego
- **Wzorce:** Kreacyjne (tworzenie), Strukturalne (kompozycja), Czynnościowe (zachowanie)
- **Architektura:** Warstwy, izolacja odpowiedzialności (np. Heksagonalna), testowalność
- **Narzędzia:** DI, Repository/UoW dla czystej architektury

