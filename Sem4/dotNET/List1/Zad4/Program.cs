using System;

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }

    public Person(string name, int age)
    {
        Name = name;
        Age = age;
    }

    public void PrintInfo()
    {
        Console.WriteLine($"Name: {Name}, Age: {Age}");
    }
}

class Program
{
    static void Main()
    {
        Person person1 = new Person("Alice", 30);
        Person person2 = new Person("Bob", 25);

        person1.PrintInfo();
        person2.PrintInfo();

        if (person1.Age > person2.Age)
        {
            Console.WriteLine($"{person1.Name} is older than {person2.Name}");
        }
        else if (person1.Age < person2.Age)
        {
            Console.WriteLine($"{person2.Name} is older than {person1.Name}");
        }
        else
        {
            Console.WriteLine($"{person1.Name} and {person2.Name} are the same age");
        }

        for (int i = 0; i < 3; i++)
        {
            Console.WriteLine($"Iteration {i + 1}");
        }

        int[] numbers = { 1, 2, 3, 4, 5 };
        Console.WriteLine("Array elements:");
        foreach (int number in numbers)
        {
            Console.WriteLine(number);
        }

        try
        {
            int x = 0;
            int y = 10 / x; // division by zero
        }
        catch (Exception ex)
        {
            Console.WriteLine($"An error occurred: {ex.Message}");
        }

        Console.Write("Inpute (hello/bye): ");
        string userInput = Console.ReadLine();
        Console.WriteLine($"You entered: {userInput}");

        switch (userInput)
        {
            case "hello":
                Console.WriteLine("Hello world!");
                break;
            case "bye":
                Console.WriteLine("Goodbye world!");
                break;
            default:
                Console.WriteLine("What!?");
                break;
        }

        Season currentSeason = Season.Summer;
        Console.WriteLine($"Current season: {currentSeason}");

        var myList = new MyList<int>();
        myList.Add(10);
        myList.Add(20);
        Console.WriteLine($"Total elements: {myList.Count}");
    }

    enum Season { Spring, Summer, Autumn, Winter }

    public class MyList<T>
    {
        private T[] array = new T[10];
        public int Count { get; private set; }

        public void Add(T item)
        {
            if (Count < array.Length)
            {
                array[Count] = item;
                Count++;
            }
            else
            {
                Console.WriteLine("List is full");
            }
        }
    }
}
