namespace Zad4v1
{
    public class Person
    {
        public string Name { get; set; }
        public string Contact { get; set; }

        public override string ToString() => $"{Name} ({Contact})";
    }

    public interface IPersonSource
    {
        List<Person> GetPersons();
    }

    public class XmlPersonSource : IPersonSource
    {
        public List<Person> GetPersons() =>
            new List<Person> {
            new Person { Name = "Anna", Contact = "anna@example.com" },
            new Person { Name = "Jan", Contact = "jan@example.com" }
            };
    }

    public class DbPersonSource : IPersonSource
    {
        public List<Person> GetPersons() =>
            new List<Person> {
            new Person { Name = "Kasia", Contact = "kasia@corp.com" },
            new Person { Name = "Tomek", Contact = "tomek@corp.com" }
            };
    }

    public abstract class PersonRegistry
    {
        protected IPersonSource _source;

        protected PersonRegistry(IPersonSource source)
        {
            _source = source;
        }

        public abstract void NotifyPersons();
    }

    public class EmailRegistry : PersonRegistry
    {
        public EmailRegistry(IPersonSource source) : base(source) { }

        public override void NotifyPersons()
        {
            foreach(var person in _source.GetPersons())
            {
                Console.WriteLine($"Email sent to {person.Name} at {person.Contact}");
            }
        }
    }

    public class SmsRegistry : PersonRegistry
    {
        public SmsRegistry(IPersonSource source) : base(source) { }

        public override void NotifyPersons()
        {
            foreach(var person in _source.GetPersons())
            {
                Console.WriteLine($"SMS sent to {person.Name} at {person.Contact}");
            }
        }
    }

}
