namespace Zad3
{
    using System;
    using System.Runtime.CompilerServices;
    using System.Threading.Tasks;

    static class TaskExtensions
    {
        public static TaskAwaiter GetAwaiter(this int miliseconds)
        {
            return Task.Delay(miliseconds).GetAwaiter();
        }
    }

    class Program3
    {
        public static async Task Main(string[] args)
        {
            Console.WriteLine("1");
            await 2000;
            Console.WriteLine("1");
        }
    }
}
