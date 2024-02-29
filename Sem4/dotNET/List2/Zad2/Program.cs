/*
finalizery czekają na garbage collector, 
idispose nie, natomiast w nim trzeba manualnie wywołać dispose() aby zwolnić zasoby
można też użyć klauzuli using, aby w chwili zakończenia następującego po niej bloku kodu, metoda Dispose() wołana jest automatycznie
*/
using Zad2;
using System;
using System.Threading;

namespace Zad2
{
    public class Finalizer
    {
        public int numer;
        public Finalizer(int Numer)
        {
            numer = Numer;
            Console.WriteLine("Utworzono obiekt finalizer {0}", numer);
        }
        ~Finalizer()
        {
            Console.WriteLine("Zniszczono obiekt finalizer {0}", numer);
        }
    }

    public class Disposabler : IDisposable
    {
        public int numer;
        public Disposabler(int Numer)
        {
            numer = Numer;
            Console.WriteLine("Utworzono obiekt disposabler {0}", numer);
        }
        public void Dispose()
        {
            Console.WriteLine("Zniszczono obiekt disposabler {0}", numer);
        }
    }

    public class Zad2
    {
        public static void Main(string[] args)
        {
            int objectNo = 10;
            Finalizer[] exampleFin = new Finalizer[objectNo];
            for (int i = 0; i < 10; i++)
                exampleFin[i] = new Finalizer(i);
            Thread.Sleep(5000);


            objectNo = 10;
            Disposabler[] exampleDis = new Disposabler[objectNo];
            for (int i = 0; i < 10; i++)
                exampleDis[i] = new Disposabler(i);
            Thread.Sleep(5000);
            for (int i = 0; i < 10; i++)
                exampleDis[i].Dispose();

            using (Disposabler exampleUDis = new Disposabler(100))
            {
                Thread.Sleep(5000);
            }

            Thread.Sleep(5000);
            GC.Collect();       // wymusza wywołanie garbage collectora, co czasem może być dobrym pomysłem
            // np gdy wiemy że będziemy korzystać z dużej ilości pamięci i chcielibyśmy wtedy nie tracić czasu na garbage collector
        }
    }
}

