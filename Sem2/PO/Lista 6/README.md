[(wróć)](../)
# Lista 6
## Wielowątkowość
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

## Serializable
Aby zapisać naszą klasę do pliku oraz móc ją później z tego odczytać możemy skorzystać (dziedziczyć/implementować) klasę Serializable.\
Dodatkowo warto zadeklarować w naszek klasie wartość klucza, za pomocą którego odbywa sie hashowanie naszej klasy (jest on domyślnie generowany w czasie kompilacji, ale jest on wtedy dość losowy i może zależeć od kompilatora, dlatego najpewniej jest mieć stały i niezmienny klucz). \
Serializable może zapisywać do pliku wszystkie klasy, które implementują jego interfejs (patrz dokumentacja).

```java
class Smth<T> implements Serializable{
    private static final long serialVersionUID = 123L;
    ...
}
```

Proces zapisywania do pliku (załóżmy że mamy już utworzoną jakąś zmienną typu _Smth_):
```java
FileOutputStream fileOutput = new FileOutputStream("Smth_class_save");
ObjectOutputStream objectOutput = new ObjectOutputStream(fileOutput);
objectOutput.writeObject(smth_object);
objectOutput.close();
fileOutput.close();
```

Process odczytywania z pliku:
```java
FileInputStream fileInput = new FileInputStream("Smth_class_save");
ObjectInputStream objectInput = new ObjectInputStream(fileInput);

Smth<?> input_Smth_object = (Smth<?>) objectInput.readObject();

objectInput.close();
fileInput.close();
```
