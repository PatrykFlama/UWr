using NUnit.Framework;
using System.Threading.Tasks;

[TestFixture]
public class BetterReusableTests
{
    [Test]
    public void BetterReusable_ShouldAcquireAndReleaseResource()
    {
        var reusable = new BetterReusable();
        Assert.DoesNotThrow(() => reusable.DoWork());
        Assert.DoesNotThrow(() => reusable.Release());
    }

    [Test]
    public void BetterReusable_ShouldThrowWhenUsingReleasedResource()
    {
        var reusable = new BetterReusable();
        reusable.Release();
        Assert.Throws<InvalidOperationException>(() => reusable.DoWork());
    }

    [Test]
    public void BetterReusable_ShouldThrowWhenReleasingTwice()
    {
        var reusable = new BetterReusable();
        reusable.Release();
        Assert.Throws<InvalidOperationException>(() => reusable.Release());
    }
}