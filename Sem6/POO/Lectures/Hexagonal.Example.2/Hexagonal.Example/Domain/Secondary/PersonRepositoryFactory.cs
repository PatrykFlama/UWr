using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Domain.Secondary
{
    public class PersonRepositoryFactory
    {
        private static Func<ApplicationContext, IPersonRepository> _create;

        public static void SetProvider( Func<ApplicationContext, IPersonRepository> create )
        {
            _create = create;
        }

        public IPersonRepository Create( ApplicationContext context)
        {
            return _create(context);
        }
    }
}
