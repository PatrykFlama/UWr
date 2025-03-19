[(back)](../)

# Wykłady


## Wykład 3 - SOLID GRASP
## **1. Responsibility-Driven Development (RDD)**
Projektowanie obiektowe polega na przypisaniu odpowiedzialności klasom i określeniu ich relacji. Skrajności:
- **Jedna ogromna klasa** – wszystko w jednej strukturze.
- **Bardzo dużo klas, każda z jedną metodą** – nadmierna fragmentacja.

## **2. GRASP (General Responsibility Assignment Software Patterns)**
Zbiór dobrych praktyk dotyczących podziału odpowiedzialności:  
1. **Creator** – klasa B tworzy instancje A, jeśli je zawiera, przechowuje lub używa.  
2. **Information Expert** – klasa posiadająca niezbędne dane powinna realizować operację.  
3. **Controller** – odbiera żądania systemowe (np. `Handler`, `Controller`).  
4. **Low Coupling** – minimalizacja powiązań między klasami.  
5. **High Cohesion** – każda klasa powinna mieć jedno, dobrze określone zadanie.  
6. **Polymorphism** – eliminacja instrukcji `if` zależnych od typu poprzez polimorfizm.  
7. **Indirection** – dodatkowa warstwa pośrednicząca dla zmniejszenia sprzężenia.  
8. **Pure Fabrication** – sztuczne klasy pomocnicze poprawiające strukturę (np. `Repository`).  
9. **Protected Variations (Law of Demeter)** – interfejsy stabilizujące punkty zmienności.

## **3. SOLID – Pięć kluczowych zasad obiektowych**
1. **SRP (Single Responsibility Principle)** – klasa powinna mieć jedną odpowiedzialność.  
2. **OCP (Open-Closed Principle)** – kod powinien być otwarty na rozszerzenia, ale zamknięty na modyfikacje.  
3. **LSP (Liskov Substitution Principle)** – obiekty klasy bazowej powinny być zastępowalne przez klasy pochodne bez zmiany zachowania.  
4. **ISP (Interface Segregation Principle)** – interfejsy powinny być podzielone na mniejsze, aby klasy nie implementowały nieużywanych metod.  
5. **DIP (Dependency Inversion Principle)** – moduły wyższego poziomu powinny zależeć od abstrakcji, a nie implementacji.

## **4. Inne zasady**
- **DRY (Don’t Repeat Yourself)** – unikanie powielania kodu.  
- **LoD (Law of Demeter)** – ograniczenie wiedzy klasy o innych klasach.  
- **DMMT (Don’t Make Me Think)** – przejrzystość kodu.  
- **DOP (Don’t Optimize Prematurely)** – unikanie przedwczesnej optymalizacji.







