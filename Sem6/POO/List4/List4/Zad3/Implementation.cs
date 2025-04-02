using System;
using System.Collections.Concurrent;


public class Reusable
{
    public void DoWork()
    {
        Console.WriteLine("Doing work...");
    }
}


public class ObjectPool
{
    private static readonly Lazy<ObjectPool> _instance = new(() => new ObjectPool());
    private readonly ConcurrentBag<Reusable> _pool = new();
    private readonly object _lock = new();

    private ObjectPool() { }
    public static ObjectPool Instance => _instance.Value;

    public Reusable AcquireReusable()
    {
        if(_pool.TryTake(out var reusable))
            return reusable;
        return new Reusable();
    }

    public void ReleaseReusable(Reusable reusable)
    {
        if(reusable == null)
            throw new ArgumentNullException(nameof(reusable));
        _pool.Add(reusable);
    }
}


public class BetterReusable
{
    private Reusable _reusable;
    private bool _released;

    public BetterReusable()
    {
        _reusable = ObjectPool.Instance.AcquireReusable();
        _released = false;
    }

    public void Release()
    {
        if(_released)
            throw new InvalidOperationException("Resource has already been released.");
        ObjectPool.Instance.ReleaseReusable(_reusable);
        _released = true;
    }

    public void DoWork()
    {
        if(_released)
            throw new InvalidOperationException("Cannot use a released resource.");
        _reusable.DoWork();
    }
}