namespace Zad8
{
    public class Program8
    {
        public static void Main(string[] args)
        {
            List<int> list = new List<int>() { 1, 2, 3, 4, 5 };
            foreach (var item in list.Select(i =>
            {
                Func<int, int> fib = null;
                fib = n => n <= 2 ? 1 : fib(n - 1) + fib(n - 2);
                return fib(i);
            }))
            {
                Console.WriteLine(item);
            }
        }
    }
}