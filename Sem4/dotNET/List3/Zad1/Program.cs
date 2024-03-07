namespace Zad1 { 
    class Program
    {
        static void Main(string[] args)
        {
            Person person = new Person() {
                    Name = "Jan",
                    Surname = "Kowalski"
                };
            XMLGenerator generator = new XMLGenerator();
            string xml = generator.GenerateXML(person);
        }
    }


    public class XMLGenerator
    {
        public string GenerateXML(IClassInfo dataObject)
        {
            // uzupełnić implementację
            throw new NotImplementedException();
        }
    }

    public interface IClassInfo
    {
        string[] GetFieldNames();
        object GetFieldValue(string fieldName);
    }


    public class Person : IClassInfo
    {
        public string Name { get; set; }
        public string Surname { get; set; }
        public string[] GetFieldNames()
        {
            return new[] { "Name", "Surname" };
        }
        public object GetFieldValue(string fieldName)
        {
            switch (fieldName)
            {
                case "Name":
                    return this.Name;
                case "Surname":
                    return this.Surname;
                default:
                    return null;
            }
            throw new NotImplementedException();
        }
    }
}