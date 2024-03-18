using System.Xml.Linq;

public class XMLGenerator
{
    public string GenerateXML(IClassInfo dataObject)
    {
        string[] fieldNames = dataObject.getFieldNames();
        XElement xElement = new("Data");
        foreach (string fieldName in fieldNames)
        {
            xElement.Add(new XElement(fieldName, dataObject.getFieldValue(fieldName)));
        }
        return xElement.ToString();
    }
}

public interface IClassInfo
{
    string[] getFieldNames();
    object getFieldValue(string fieldName);
}

public class Person : IClassInfo
{
    public string Name { get; set; }
    public string Surname { get; set; }
    public string[] getFieldNames()
    {
        return new string[] { "Name", "Surname" };
    }
    public object getFieldValue(string fieldName)
    {
        if (fieldName == "Name")
            return Name;
        else if (fieldName == "Surname")
            return Surname;
        else
            throw new NotImplementedException();
    }
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