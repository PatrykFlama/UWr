using System;
using System.Collections.Generic;

//// Testowanie fabryki
//class Program
//{
//    static void Main()
//    {
//        ShapeFactory factory = new ShapeFactory();
//        factory.RegisterWorker(new SquareFactoryWorker());

//        IShape square = factory.CreateShape("Square", 5);
//        square.Draw();

//        factory.RegisterWorker(new RectangleFactoryWorker());
//        IShape rect = factory.CreateShape("Rectangle", 3, 5);
//        rect.Draw();
//    }
//}