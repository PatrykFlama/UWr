# Lista 5 - Java
## Główna funkcja programu w Javie
```java
class Program {
    public static void main(String[] args) {
        System.out.println("Hello World!\n");
        System.out.println("Hello World!\n");
    }
}
```

## Dziedziczenie klas (_extents_)
Dziedziczenie klas / podklasy są wykonywane za pomocą słowa kluczowego _extents_
```java
class Father{
    void func(){}
}
class Child extends Father{
    void func(){
        somestuff;
    }
}
```
Możemy sprawdzać instancją jakiej klasy jest dany obiekt za pomocą _instanceof_
```java
SomeClass some_object;
if(some_object instanceof SomeClass){
    dosomething;
}
```

## Rzucanie wyjątków
W javie mamy możliwość korzystania z _try-catch-finally_ gdzie try próbuje coś wykonać, catch wyłapuje ewentualne błędy, a kod w finally wykonuje się niezależnie od tego czy błąd został złapany czy nie.

Aby rzucić wyjątkiem w funkcji musimy wpierw zakomunikować jej że wystąpi taka możliwość
```java
void func() throws Exception{
    throw new Exception("My message");
}
```
