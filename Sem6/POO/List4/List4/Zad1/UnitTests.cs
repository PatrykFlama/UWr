
// Testy jednostkowe
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Threading.Tasks;


[TestClass]
public class SingletonTests
{
    [TestMethod]
    public void ProcessSingleton_ShouldReturnSameInstance()
    {
        var instance1 = ProcessSingleton.Instance;
        var instance2 = ProcessSingleton.Instance;
        Assert.AreEqual(instance1, instance2);
    }

    [TestMethod]
    public void ThreadSingleton_ShouldReturnDifferentInstancesPerThread()
    {
        ThreadSingleton instance1 = null, instance2 = null;

        var thread1 = new Thread(() => instance1 = ThreadSingleton.Instance);
        var thread2 = new Thread(() => instance2 = ThreadSingleton.Instance);

        thread1.Start();
        thread2.Start();

        thread1.Join();
        thread2.Join();

        Assert.AreNotEqual(instance1, instance2);
    }

    [TestMethod]
    public async Task TimedSingleton_ShouldExpireAfterFiveSeconds()
    {
        var instance1 = TimedSingleton.Instance;
        await Task.Delay(6000);
        var instance2 = TimedSingleton.Instance;
        Assert.AreNotEqual(instance1, instance2);
    }
}
