/*
Patryk Flama - lista 3 zadanie 1
kompilacja: mcs -reference:lista.dll zad1-main.cs
*/


using System;

class Program{
    public static void Main(string[] args){
        List<int> list = new List<int>();
        Console.WriteLine("Popping from front");
        list.push_back(1);
        list.push_back(2);
        list.push_front(6);
        list.push_back(3);
        for(int i = 0; i < 4; i++) {
            Console.Write(list.pop_front());
            Console.Write(' ');
        }
        Console.WriteLine();

        Console.WriteLine("Popping from back");
        list.push_back(1);
        list.push_back(2);
        list.push_front(6);
        list.push_back(3);
        for(int i = 0; i < 4; i++) {
            Console.Write(list.pop_back());
            Console.Write(' ');
        }
        Console.WriteLine();
    }
}