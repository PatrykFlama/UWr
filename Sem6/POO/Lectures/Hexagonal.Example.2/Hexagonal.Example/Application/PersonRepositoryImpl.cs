using Domain.Secondary;
using Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Application
{
    public class PersonRepositoryImpl : IPersonRepository
    {
        private ModelDbContext context;

        public PersonRepositoryImpl( ModelDbContext context )
        {
            this.context = context;
        }

        public IEnumerable<Person> GetAll()
        {
            return context.Persons.ToList();
        }

        public Person GetByID( int ID )
        {
            return context.Persons.FirstOrDefault( p => p.ID == ID );
        }

        public void Insert( Person person )
        {
            context.Persons.Add( person );
            context.SaveChanges();
        }

        public Person New()
        {
            return new Person();
        }

        public void Update( Person person )
        {
            context.SaveChanges();
        }
    }
}
