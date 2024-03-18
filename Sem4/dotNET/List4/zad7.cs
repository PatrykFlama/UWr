namespace Zad7 {
    public class Program7
    {
        public static void Main(string[] args)
        {
            var item = new { Name = "Person1", Age = 1 };
            var item2 = new { Name = "Person2", Age = 2 };

            List<object> list = new List<object>() { item, item2 };
            list.Add(new { Name = "Person3", Age = 3 });
            foreach (var i in list)
            {
                Console.WriteLine(i);
            }
        }

    }
}