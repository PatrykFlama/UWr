[(wróć)](../)
# Lista 4
## Implementowanie Enumeratorów w klasie
Aby dodać do naszej klasy możliwoś iterowania się po niej możemy zintegrować ją z interfejsem enumeratora. Polega to na:
* dziedziczeniu _IEnumerable_ w klasie, która ma być iterowalna, dodając do niej adekwatną funkcję, wywołującą enumerator naszej funkcji (osobna klasa):
```cs
public IEnumerator GetEnumerator(){
    return new EnumeratorNaszejFunkcji();
}
```
* utworzeniu osobnej klasy dziedziczącej _IEnumerator_, która zawiera następujące funkcje/elementy:
  * public bool MoveNext();     przesuwa pozycję iteratora do przodu o jeden krok, zwraca fałsz jeżeli dotarła do końca strumienia, po którym się iterujemy, w p.p. prawdę
  * public void Reset();        resetuje pozycję iteratora, czyli ustawia go na początek
  * public object Currentget{return aktualna_wartość;}       zwraca wartość aktualnego elementu, tego na którym aktualnie stoi nasz iterator

W efekcie możemy iterować sie po naszej klasie:
```cs
foreach(Type element in new NaszaKlasa()){
    Console.WriteLine(element);
}
```
