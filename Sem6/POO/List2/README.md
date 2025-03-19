# List 2

| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|---|---|---|---|---|---|---|---|
| X | X | X | X | X | X | X | X |


## Zad 1
```mermaid
classDiagram
    class ICommand {
        <<interface>>

        +Execute(string CommandName)
    }

    class AbstractCommand {
        <<abstract>>

        -int commandCount
        #string commandState
        +string commandName
        -void commandBuilder()
        +abstract void Execute(string CommandName)
    }

    class ConcreteCommand {
        #CommandStepBuilder commandBuilder
        +void Execute(string CommandName)
    }

    class CommandStepBuilder {
        +const int MAXSTEPS = 10
        +static int StepCount
    }

    ICommand <|.. AbstractCommand
    AbstractCommand <|-- ConcreteCommand
    ConcreteCommand --> CommandStepBuilder
```



## Zad 2
```mermaid
classDiagram
    class ConcreteCommand {
        +commandName : string = "SampleCommand"
        +commandState : string = "Initialized"
        +commandBuilder : CommandStepBuilder
    }

    class AnotherCommand {
        +commandName : string = "AnotherCommand"
        +commandState : string = "Pending"
        +commandBuilder : CommandStepBuilder
    }

    class CommandStepBuilder {
        +StepCount : int = 5
        +MAXSTEPS : int = 10
    }

    ConcreteCommand --> CommandStepBuilder
    AnotherCommand --> CommandStepBuilder
```


## Zad 3
```mermaid
stateDiagram
    [*] --> Idle
    Idle --> SelectingDrink : Wybór napoju
    SelectingDrink --> Payment : Płatność
    Payment --> PaymentAccepted : Płatność zaakceptowana
    Payment --> PaymentFailed : Błąd płatności
    PaymentFailed --> Idle : Powrót do stanu początkowego

    PaymentAccepted --> Brewing : Przygotowywanie kawy
    Brewing --> Dispensing : Wydawanie napoju
    Dispensing --> Idle : Powrót do stanu początkowego
```


## Zad 4
### Diagram bez paryycji
```mermaid
graph TD
    A[Start] --> B[Wybór napoju]
    B --> C[Wprowadzenie płatności]
    C --> D[Weryfikacja płatności]
    
    D -->|Płatność zaakceptowana| E[Przygotowywanie kawy]
    E --> F[Wydawanie napoju]
    E -->|Brak kawy| L[Zwrot środków]
    F --> G[Koniec]
    L --> G
    
    D -->|Brak środków| H[Komunikat o błędzie]
    H --> C
    
    D -->|Błędna kwota| I[Żądanie poprawnej kwoty]
    I --> C
    
    D -->|Błędny PIN| J[Żądanie ponownego wprowadzenia PINu]
    J --> C
```

### Diagram z partycjami
```mermaid
graph TD
    subgraph Użytkownik
        A[Start] --> B[Wybór napoju]
        B --> C[Wprowadzenie płatności]
        C --> D[Podanie PINu]
    end

    subgraph Automat
        D --> E[Weryfikacja płatności]

        E -->|Płatność zaakceptowana| F[Przygotowywanie kawy]
        F --> G[Wydawanie napoju]
        G --> H[Koniec]

        E -->|Brak kawy| L[Zwrot środków]
        L --> H

        E -->|Brak środków| I[Komunikat o błędzie]
        I --> C

        E -->|Błędna kwota| J[Żądanie poprawnej kwoty]
        J --> C

        E -->|Błędny PIN| K[Żądanie ponownego wprowadzenia PINu]
        K --> C
    end
```

## Zad 5
```mermaid
sequenceDiagram
    participant Użytkownik
    participant Interfejs_UI as Interfejs użytkownika
    participant Repozytorium as Repozytorium danych

    Użytkownik->>Interfejs_UI: Wprowadza dane rejestracyjne
    Interfejs_UI->>Interfejs_UI: Walidacja danych
    Interfejs_UI-->>Użytkownik: Błąd walidacji? (jeśli tak, wyświetlenie błędu)
    
    Interfejs_UI->>Repozytorium: Sprawdzenie unikalności emaila
    Repozytorium-->>Interfejs_UI: Email zajęty? (jeśli tak, wyświetlenie błędu)

    Interfejs_UI->>Repozytorium: Zapisanie nowego użytkownika
    Repozytorium-->>Interfejs_UI: Potwierdzenie zapisu

    Interfejs_UI-->>Użytkownik: Rejestracja zakończona sukcesem
```


## Zad 6
```mermaid
sequenceDiagram
    participant Zadanie1
    participant A
    participant B
    participant C

    Zadanie1->>A: Wykonaj(v)
    A-->>B: Oblicz(x) (jeśli x < 10)
    A-->>C: Oblicz(x) (jeśli x >= 10)
```

### czy można narysować jednoznaczny diagram?
można narysować częściowo jednoznacznie – można pokazać kolejność wywołań, ale nie określić pełnego przepływu danych i efektów działania metod  

## Zad 7
```cpp
class Student {
    void show() {
        userWebPage.getStudentGradeInfo();
    }
}

class UserWebPage {
    void getStudentGradeInfo() {
        gradeController.getCourses();
        for each course in courses {
            mark = course.getMark(student);
            mark.getValue();
        }
        pageLayout();
    }
}

class GradeController {
    List<Course> getCourses() {
        return student.getCourses();
    }
}

class Student {
    List<Course> getCourses() {
        return enrolledCourses;
    }
}

class Course {
    Mark getMark(Student student) {
        return student.getMark(this);
    }
}

class Mark {
    int getValue() {
        return value;
    }
}
```

### czy można to zrobić jednoznacznie?
* nie ma określonej struktury klasy `Mark` – zakładamy, że przechowuje ocenę jako _int_
* nie wiadomo, czy `getCourses()` zwraca kursy konkretnego studenta – domyślamy się że pobiera je z obiektu `Student`
* nie ma informacji, jak działa `pageLayout()` – zakładamy, że kończy proces wyświetlania ocen
* nie widać obsługi błędów – co się dzieje gdy student nie ma kursów
