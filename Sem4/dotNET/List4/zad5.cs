namespace Zad5 {
    public class Program5
    {
        public static void Main(string[] args)
        {
            // read data
            var daneOsobowe = File.ReadAllLines("C:\\Users\\patry\\Documents\\Programming\\UWr\\Sem4\\dotNET\\List4\\zad5_dos.txt")
                      .Select(line => line.Split(' '))
                      .Select(parts => new
                      {
                          Imię = parts[0].Trim(),
                          Nazwisko = parts[1].Trim(),
                          PESEL = parts[2].Trim()
                      });

            var daneKonta = File.ReadAllLines("C:\\Users\\patry\\Documents\\Programming\\UWr\\Sem4\\dotNET\\List4\\zad5_kon.txt")
                                .Select(line => line.Split(' '))
                                .Select(parts => new
                                {
                                    PESEL = parts[0].Trim(),
                                    NumerKonta = parts[1].Trim()
                                });

            // operate
            var połączoneDane = from osoba in daneOsobowe
                                join konto in daneKonta
                                on osoba.PESEL equals konto.PESEL
                                select new
                                {
                                    osoba.Imię,
                                    osoba.Nazwisko,
                                    osoba.PESEL,
                                    konto.NumerKonta
                                };

            // res
            foreach (var rekord in połączoneDane)
            {
                Console.WriteLine($"{rekord.Imię}, {rekord.Nazwisko}, {rekord.PESEL}, {rekord.NumerKonta}");
            }
        }
    }
}