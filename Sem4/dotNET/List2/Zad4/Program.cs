namespace Zad4 { 
    class Program
    {
        static void Main(string[] args)
        {
            Person p = new Person();
            Observer o = new Observer();
            Observer o2 = new Observer();
            Observer2 o4 = new Observer2();

            p.LoggersEvent += o.Event;
            p.LoggersEvent += o2.Event;
            p.LoggersEvent += o4.TellTheTruth;

            p.Name = "Yep";
            p.Surname = "Nope";

            p.Name = "John";
            p.Surname = "what";
        }
    }


    class Observer
    {
        public void Event(string val1, string val2)
        {
            Console.WriteLine($"value changed: {val1} {val2}");
        }
    }

    class Observer2
    {
        public void TellTheTruth(string truth, string lie) 
        {
            Console.WriteLine($"and the truth is {truth}");
        }
    }

    class Person
    {
        public delegate void Loggers(string val1, string val2);
        public event Loggers LoggersEvent;

        private string _name, _surname;
        public string Name
        {
            get
            {
                return _name;
            }
            set
            {
                if(value != _name)
                    LoggersEvent.Invoke(value, _surname);
                _name = value;
            }
        }
        public string Surname
        {
            get
            {
                return _surname;
            }
            set
            {
                if(value != _surname)
                    LoggersEvent.Invoke(_name, value);
                _surname = value;
            }
        }
    }
}