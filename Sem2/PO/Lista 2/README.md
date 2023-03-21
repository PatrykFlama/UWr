# Lista 2
## Kompilacja C#
Do kompilacji plików c# można użyć .NET (duże narzędzie, które tworzy dużo plików - dobrze się sprawdza przy projekatch, gorzej przy pojedynczych plikach) lub kompilatora _mcs mono_.

#### Kompilacja w mcs mono
Działa podobnie jak gcc - _mcs nazwapliku.cs_

#### kompilacja w .NET
* initialize project with _dotnet new console_
* _dotnet run_ to run
* _dotnet restore_ to ???

## Co warto wynieść z tej listy
Dziedziczenie klas oraz override konkretych funkcji z klasy-ojca: dziedziczeniem klas możemy 'przejmować' funkcje z klasy ojca, dzięki czemu możemy się mniej napisać i nie powtarzać takich samych bloków kodu. W przypadku niewielkich różnic, lub gdy taka potrzeba wystąpi z powodu sposobu działania klasy, możemy skorzystać z *override*, dzięki ktróremu możemy nadpisać działanie konkretnej funkcji z klasy ojca w naszej klasie (dziecku).
