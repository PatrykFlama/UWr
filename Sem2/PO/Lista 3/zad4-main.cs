/*
Patryk Flama - lista 3 zadanie 4
kompilacja: mcs -reference:lista.dll zad4-main.cs
*/
using System;

class Program{
    public static void Main(string[] args){
        Vector v1 = new Vector(4);
        Vector v2 = new Vector(4);
        
        v1.set_vector(new float[] {1, 2, 3, 4});
        v2.set_vector(new float[] {4, 3, 2, 1});
        Console.WriteLine("Created vectors:");
        v1.print();
        v2.print();
        Console.WriteLine("V1 = V1 + V2");
        v1 = v1 + v2;
        v1.print();
        Console.WriteLine("V1 = V1 * 0.5");
        v1 = v1 * 0.5f;
        v1.print();
        Console.WriteLine("Dot product");
        Console.WriteLine(v1 * v2);
        Console.WriteLine("Norm from V1");
        Console.WriteLine(v1.norm());
    }
};