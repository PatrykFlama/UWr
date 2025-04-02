using System;
using System.Threading;


//class Program
//{
//    static void Main()
//    {
//        Console.WriteLine("Process Singleton: " + (ProcessSingleton.Instance == ProcessSingleton.Instance));

//        ThreadSingleton instance1 = null, instance2 = null;
//        var thread1 = new Thread(() => instance1 = ThreadSingleton.Instance);
//        var thread2 = new Thread(() => instance2 = ThreadSingleton.Instance);
//        thread1.Start();
//        thread2.Start();
//        thread1.Join();
//        thread2.Join();
//        Console.WriteLine("Thread Singleton (different threads): " + (instance1 != instance2));

//        var timed1 = TimedSingleton.Instance;
//        Thread.Sleep(6000);
//        var timed2 = TimedSingleton.Instance;
//        Console.WriteLine("Timed Singleton expired: " + (timed1 != timed2));
//    }
//}
