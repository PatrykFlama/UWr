using Microsoft.VisualStudio.TestTools.UnitTesting;

[TestClass]
public class ShapeFactoryTests
{
    [TestMethod]
    public void Should_Create_Square_When_Registered()
    {
        var factory = new ShapeFactory();
        factory.RegisterWorker(new SquareFactoryWorker());

        var shape = factory.CreateShape("Square", 5);

        Assert.AreNotEqual(shape, null);
    }

    [TestMethod]
    public void Should_Create_Rectangle_When_Registered()
    {
        var factory = new ShapeFactory();
        factory.RegisterWorker(new RectangleFactoryWorker());

        var shape = factory.CreateShape("Rectangle", 3, 5);

        //Assert.That(shape, Is.InstanceOf<Rectangle>());
        Assert.AreNotEqual(shape, null);
    }

    [TestMethod]
    public void Should_Throw_Exception_When_Shape_Not_Registered()
    {
        var factory = new ShapeFactory();

        Assert.Throws<ArgumentException>(() => factory.CreateShape("Triangle", 5));
    }
}