using Domain.Secondary;
using Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Domain.Primary
{
    public class PersonListUseCase
    {
        public PersonListUseCaseResults Execute( PersonListUseCaseParameters parameters, ApplicationContext context )
        {
            var repository = new PersonRepositoryFactory().Create(context);

            var persons = repository.GetAll();

            return new PersonListUseCaseResults()
            {
                Persons = persons
            };
        }
    }

    public class PersonListUseCaseParameters
    {

    }

    public class PersonListUseCaseResults
    {
        public IEnumerable<Person> Persons { get; set; }
    }
}
