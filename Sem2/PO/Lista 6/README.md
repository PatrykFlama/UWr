[(wróć)](../)
# Lista 6 - wielowątkowość
Aby klasa mogła obsługiwać wielowątkowość, powinna ona dziedziczyć _Thread_ oraz implementować funkcję _public void run()_:
```java
class Smth extends Thread{
    public void run(){
        compute;
    }
}
```
Teraz, aby odaplić ją w osobnym wolnym wątku wywołujemy funkcję _start()_ z naszej klasy:
```java
Smth s;
s.start();
```
Możemy wywołać funkcję _join()_, która poczeka na zakączenie wątku.\
Za pomocą _setPriority(*\<int\>*)_ możemy ustawić priotytet poszczególnych instancji klas, co wpłynie na priorytet danego wątku. Priorytety są od 1 do 10.\
Aby upewnić się że jakieś części kodu wywołają się w odpowiedniej kolejności (np inny wątek nie będzie w tym samym czasie nadpisywać tej samej tablicy) możemy skorzystać z sekcji _synchronized(obj){}_
```java
class Smth{
    public void smth(){
        synchronized(this){
            dosmth;
        }
    }
}
```
lub
```java
class Smth{
    public synchronized void smth(){
        dosmth;
    }
}
```
dzięki czemu dany wątek może założyć 'blokadę' na wątek, aby mieć go na wyłączność i upewnić się, że wielowątkowość nie zepsuje działania kodu. Jeżeli inny wątek będzie chciał skorzystać z obiektu, albo też założyć na niego blokadę, będzie musiał poczekać.
<> TODO serializable
