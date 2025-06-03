using Domain.Secondary;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Domain.Primary
{
    public class PersonAddUseCase
    {
        public PersonAddUseCaseResults Execute( PersonAddUseCaseParameters parameters, ApplicationContext context )
        {
            var repository = new PersonRepositoryFactory().Create(context);

            var person     = repository.New();
            person.Name    = parameters.Name;
            person.Surname = parameters.Surname;

            repository.Insert( person );

            return new PersonAddUseCaseResults()
            {
                Status = true
            };
        }
    }

    public class PersonAddUseCaseParameters
    {
        public string Name { get; set; }
        public string Surname { get; set; }
    }

    public class PersonAddUseCaseResults
    {
        public bool Status { get; set; }
    }
}
