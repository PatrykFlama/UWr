using System;
using System.Threading;

// Singleton dla całego procesu
public class ProcessSingleton
{
    private static readonly ProcessSingleton _instance = new ProcessSingleton();
    private ProcessSingleton() { }
    public static ProcessSingleton Instance => _instance;
}

// Singleton dla każdego wątku
public class ThreadSingleton
{
    private static readonly ThreadLocal<ThreadSingleton> _instance =
        new ThreadLocal<ThreadSingleton>(() => new ThreadSingleton());

    private ThreadSingleton() { }

    public static ThreadSingleton Instance => _instance.Value;
}

// Singleton z czasem życia maksymalnie 5 sekund
public class TimedSingleton
{
    private static TimedSingleton _instance;
    private static readonly object _lock = new object();
    private static DateTime _creationTime;

    private TimedSingleton()
    {
        _creationTime = DateTime.UtcNow;
    }

    public static TimedSingleton Instance
    {
        get
        {
            if(_instance == null || (DateTime.UtcNow - _creationTime).TotalSeconds > 5)
            {
                lock(_lock)
                {
                    if(_instance == null || (DateTime.UtcNow - _creationTime).TotalSeconds > 5)
                    {
                        _instance = new TimedSingleton();
                    }
                }
            }
            return _instance;
        }
    }
}
