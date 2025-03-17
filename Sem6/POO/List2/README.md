# List 2

| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|---|---|---|---|---|---|---|---|
|   |   |   |   |   |   |   |   |


## Zad 1
```mermaid
classDiagram
    class ICommand {
        +Execute(string CommandName)
    }

    class AbstractCommand {
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

