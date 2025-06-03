using Domain.Secondary;
using Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Domain.Primary
{
    public class PersonOneUseCase
    {
        public PersonOneUseCaseResults Execute( PersonOneUseCaseParameters parameters, ApplicationContext context )
        {
            var repository = new PersonRepositoryFactory().Create(context);

            var person = repository.GetByID( parameters.ID );

            return new PersonOneUseCaseResults()
            {
                Person = person
            };
        }
    }

    public class PersonOneUseCaseParameters
    {
        public int ID { get; set; }
    }

    public class PersonOneUseCaseResults
    {
        public Person Person { get; set; }
    }
}
