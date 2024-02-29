namespace Zad1
{
    class Zad1
    {

        //! modyfikator static dla klas
        /*
            only static members
            cant be instantiated
            is sealed (aka u cant inherit)
            cant contain public constructors
         */

        //! modyfikator static dla składowych klas (pól, metod)
        // pola dla całej klasy - nie dla jej istancji
        static class Statics
        {
            public static int n;
        }

        //! modyfikator sealed dla klas
        // tak klasa nie może być rodzicem przy dziedziczeniu
        sealed class SealedClass
        {
            public static int n;
        }

        //! modyfikator abstract dla klas
        // cały interfejs, wszystkie metody abstrakcyjne

        //! modyfikator abstract dla składowych klas(metod)
        // do iterfejsów - nie ma ciała TRZEBA zaimplementować, z użyciem override

        abstract class AbstractS
        {
            abstract public int somefunc();
        }

        //! słowa kluczowe virtual i override dla składowych klas (metod)
        // virtual wymusza użycie override żeby nadpisać, może mieć ciało
        class Virtuals
        {
            virtual public int log(int x)
            {
                return x;
            }
        }
        class InheritedV : Virtuals
        {
            override public int log(int x)
            {
                return x + 1;
            }
        }

        //! słowo kluczowe partial w definicji klasy
        // można rozbić implementację klasy
        partial class PartialEg
        {
            public static int n;
        }
        partial class PartialEg
        {
            public void somefunc(in int x)
            {
                PartialEg.n = x;
            }
        }

        //! słowo kluczowe readonly w deklaracji pola klasy
        // ze zmiennej można tylko czytać - niemodyfikowalna kopia
        class ReadME
        {
            public readonly int n;
            ReadME(int n)
            {
                this.n = n;
            }
            void func(int x) // jak referencja w C++
            {
                x = n;
                //n = x error
            }
        }

        //! modyfikatory in, ref oraz out na liście parametrów metody
        /*
        IN - niemodyfikowalna referencja
        REF - referencja
        OUT - referencja TRZEBA zmodyfikować
         */
        public void ProcessInput(in int value, ref int yes, out int nope)
        {
            // value += 1; //ERR
            yes = value;
            nope = yes;     // bez tego błąd
        }

        //! modyfikator params na liście parametrów metody
        // params zaznacza że chcemy przekazać tablicę parametrów ze zmienną liczbą arguemntów
        public int Sum(params int[] numbers)
        {
            int sum = 0;
            foreach (int num in numbers)
                sum += num;
            return sum;
        }
    }
}
