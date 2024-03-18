using System;

namespace Zad2 {
    public class Program2
    {
        public static void Main(string[] args)
        {
            string[] lines = File.ReadAllLines("C:\\Users\\patry\\Documents\\Programming\\UWr\\Sem4\\dotNET\\List4\\zad2.txt");

            var query = from liczba in lines
                        where int.TryParse(liczba, out _) && int.Parse(liczba) > 100
                        orderby int.Parse(liczba) descending
                        select int.Parse(liczba);

            var result = lines.Where(liczba => int.TryParse(liczba, out int num) && num > 100)
                              .Select(liczba => int.Parse(liczba))
                              .OrderByDescending(num => num);

            Console.WriteLine("LINQ:");
            foreach (var number in query)
            {
                Console.WriteLine(number);
            }

            Console.WriteLine("\nmetody LINQ:");
            foreach (var number in result)
            {
                Console.WriteLine(number);
            }
        }
    }
}