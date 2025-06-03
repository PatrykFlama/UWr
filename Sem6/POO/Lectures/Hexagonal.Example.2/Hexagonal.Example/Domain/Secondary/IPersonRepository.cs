using Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Domain.Secondary
{
    public interface IPersonRepository
    {
        Person New();

        void Insert( Person person );
        void Update( Person person );

        IEnumerable<Person> GetAll();

        Person GetByID( int ID );
    }
}
