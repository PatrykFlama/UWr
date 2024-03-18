namespace Zad6 {
    public class Program6
    {
        public static void Main(string[] args)
        {
            var lines = File.ReadAllLines("C:\\Users\\patry\\Documents\\Programming\\UWr\\Sem4\\dotNET\\List4\\zad6.txt");

            var topClients = lines.Select(line => line.Split(' ')[1])
                                  .GroupBy(ip => ip)
                                  .Select(group => new { IP = group.Key, Requests = group.Count() })
                                  .OrderByDescending(client => client.Requests) 
                                  .Take(3);

            foreach (var client in topClients)
            {
                Console.WriteLine($"{client.IP} {client.Requests}");
            }
        }
    }
}