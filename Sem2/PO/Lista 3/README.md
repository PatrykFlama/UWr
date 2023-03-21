# Lista 3
## Podział programu na biblioteki oraz kod/program głowny
Aby zwiększyć czytelność, wspomóc ogranizację kodu oraz przyspieszyć czas kompilacji (szczególnie dużych projektów) możemy wydzielić z naszego programu konkretne klasy oraz funkcje i zamknąć je w osobnym pliku-biblioteki, który możemy od razu skompilować.
##### Jak to zrobić w _mcs mono_
Kompilacja biblioteki:
```sh
mcs -target:library -out:biblioteka.dll biblioteka.cs
```

Kompilacja programu:
```sh
mcs -reference:biblioteka.dll main.cs
```
