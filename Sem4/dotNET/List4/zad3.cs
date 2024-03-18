namespace Zad3 { 
    public class Program3
    {
        public static void Main(string[] args)
        {
            string[] names = File.ReadAllLines("C:\\Users\\patry\\Documents\\Programming\\UWr\\Sem4\\dotNET\\List4\\zad3.txt");

            var firstLetters = names.Where(name => !string.IsNullOrWhiteSpace(name))
                                    .GroupBy(name => name[0])
                                    .OrderBy(group => group.Key)
                                    .Select(group => group.Key);

            foreach (var letter in firstLetters)
            {
                Console.WriteLine(letter);
            }
        }
    }
}