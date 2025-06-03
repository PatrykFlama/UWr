using Domain.Secondary;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Domain.Primary
{
    public class PersonModifyUseCase
    {
        public PersonModifyUseCaseResults Execute( PersonModifyUseCaseParameters parameters, ApplicationContext context )
        {
            var repository = new PersonRepositoryFactory().Create(context);

            var person     = repository.GetByID( parameters.ID );
            if ( person != null )
            {
                person.Name = parameters.Name;
                person.Surname = parameters.Surname;

                repository.Update( person );

                return new PersonModifyUseCaseResults()
                {
                    Status = true
                };
            }
            else
            {
                return new PersonModifyUseCaseResults()
                {
                    Status = false
                };
            }
        }
    }

    public class PersonModifyUseCaseParameters
    {
        public int ID { get; set; }
        public string Name { get; set; }
        public string Surname { get; set; }
    }

    public class PersonModifyUseCaseResults
    {
        public bool Status { get; set; }
    }
}
