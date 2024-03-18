using System;
using System.IO;
using System.Linq;

namespace Zad4 {
    public class Program4
    {
        public static void Main(string[] args)
        {
            string folderPath = "C:\\Users\\patry\\Documents\\Programming\\UWr\\Sem4\\dotNET\\List4";
            if (!Directory.Exists(folderPath))
            {
                Console.WriteLine("Podana ścieżka nie istnieje lub nie jest katalogiem.");
                return;
            }

            long totalLength = Directory.GetFiles(folderPath)
                                        .Select(file => new FileInfo(file).Length)
                                        .Aggregate((acc, length) => acc + length);

            Console.WriteLine($"suma długości plików w folderze: {totalLength} bajtów.");
        }
    }
}