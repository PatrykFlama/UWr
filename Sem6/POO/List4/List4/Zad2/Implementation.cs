using System;
using System.Collections.Generic;



public interface IShape
{
    void Draw();
}



public class Square : IShape
{
    private int _side;
    public Square(int side) => _side = side;
    public void Draw() => Console.WriteLine($"Drawing a Square with side {_side}");
}

public class Rectangle : IShape
{
    private int _width, _height;
    public Rectangle(int width, int height)
    {
        _width = width;
        _height = height;
    }
    public void Draw() => Console.WriteLine($"Drawing a Rectangle with width {_width} and height {_height}");
}



public interface IShapeFactoryWorker
{
    string ShapeName { get; }
    IShape Create(params object[] parameters);
}



public class SquareFactoryWorker : IShapeFactoryWorker
{
    public string ShapeName => "Square";
    public IShape Create(params object[] parameters) => new Square((int)parameters[0]);
}

public class RectangleFactoryWorker : IShapeFactoryWorker
{
    public string ShapeName => "Rectangle";
    public IShape Create(params object[] parameters) => new Rectangle((int)parameters[0], (int)parameters[1]);
}


public class ShapeFactory
{
    private readonly Dictionary<string, IShapeFactoryWorker> _workers = new();

    public void RegisterWorker(IShapeFactoryWorker worker)
    {
        _workers[worker.ShapeName] = worker;
    }

    public IShape CreateShape(string shapeName, params object[] parameters)
    {
        if(_workers.TryGetValue(shapeName, out var worker))
        {
            return worker.Create(parameters);
        }
        throw new ArgumentException($"No worker registered for shape: {shapeName}");
    }
}
