using System.Reflection.Emit;
using System;
using System.Xml.Linq;

public class XMLGenerator
{
    public string GenerateXML(object dataObject)
    {
        XElement xElement = new("Data");
        foreach (var property in dataObject.GetType().GetProperties())
        {
            xElement.Add(new XElement(property.Name, property.GetValue(dataObject)));
        }
        return xElement.ToString();
    }
}


public class Person
{
    public string Name { get; set; }
    public string Surname { get; set; }

}


public class Program
{
    public static void Main(string[] args)
    {
        Person person = new Person();
        person.Name = "John";
        person.Surname = "Doe";
        XMLGenerator xmlGenerator = new();
        string xml = xmlGenerator.GenerateXML(person);
        Console.WriteLine(xml);
    }
}