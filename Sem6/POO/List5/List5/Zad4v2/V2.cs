using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Zad4v2
{
    //public class Program
    //{
    //    static void Main(string[] args) { }
    //}


    public class Person
    {
        public string Name { get; set; }
        public string Contact { get; set; }

        public override string ToString() => $"{Name} ({Contact})";
    }

    public interface IPersonNotifier
    {
        void Notify(Person person);
    }

    public class EmailNotifier : IPersonNotifier
    {
        public void Notify(Person person)
        {
            Console.WriteLine($"Email sent to {person.Name} at {person.Contact}");
        }
    }

    public class SmsNotifier : IPersonNotifier
    {
        public void Notify(Person person)
        {
            Console.WriteLine($"SMS sent to {person.Name} at {person.Contact}");
        }
    }

    public abstract class PersonRegistry
    {
        protected IPersonNotifier _notifier;

        protected PersonRegistry(IPersonNotifier notifier)
        {
            _notifier = notifier;
        }

        public abstract List<Person> GetPersons();

        public void NotifyPersons()
        {
            foreach(var person in GetPersons())
            {
                _notifier.Notify(person);
            }
        }
    }

    public class XmlPersonRegistry : PersonRegistry
    {
        public XmlPersonRegistry(IPersonNotifier notifier) : base(notifier) { }

        public override List<Person> GetPersons()
        {
            return new List<Person>
        {
            new Person { Name = "Anna", Contact = "anna@example.com" },
            new Person { Name = "Jan", Contact = "jan@example.com" }
        };
        }
    }

    public class DbPersonRegistry : PersonRegistry
    {
        public DbPersonRegistry(IPersonNotifier notifier) : base(notifier) { }

        public override List<Person> GetPersons()
        {
            return new List<Person>
        {
            new Person { Name = "Kasia", Contact = "kasia@corp.com" },
            new Person { Name = "Tomek", Contact = "tomek@corp.com" }
        };
        }
    }

}
