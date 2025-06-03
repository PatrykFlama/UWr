using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MVP.Example.Refactored.Model
{
    public interface IPersonRepository
    {
        IEnumerable<Person> GetAll();
        void Insert(Person person);
    }
}
