using System;

public class AccessModifiersDemo
{
    // can be accessed from everywhere
    public string publicField = "Public field";

    // can be accessed only in the class and its subclasses
    protected string protectedField = "Protected field";

    // cant be accessed from different assemblies (feg libraries)
    internal string internalField = "Internal field";

    // can be accessed only from this class
    private string privateField = "Private field";

    // same goes for methods
    public void PublicMethod()
    {
        Console.WriteLine("Public method called");
    }

    protected void ProtectedMethod()
    {
        Console.WriteLine("Protected method called");
    }

    internal void InternalMethod()
    {
        Console.WriteLine("Internal method called");
    }

    private void PrivateMethod()
    {
        Console.WriteLine("Private method called");
    }
}

// DEMO:
public class SubClass : AccessModifiersDemo
{
    public void AccessProtectedField()
    {
        Console.WriteLine("Protected field in subclass: " + protectedField);
    }
}

class Program
{
    static void Main(string[] args)
    {
        AccessModifiersDemo demo = new AccessModifiersDemo();

        try
        {
            //Console.WriteLine("Private field: " + demo.privateField);
        }
        catch(Exception e)
        {
            Console.WriteLine($"An error occurred: {e.Message}");
        }

        Console.WriteLine("Public field: " + demo.publicField);
        Console.WriteLine("Internal field: " + demo.internalField);

        demo.PublicMethod();
        demo.InternalMethod();

        SubClass sub = new SubClass();
        sub.AccessProtectedField();
    }
}
