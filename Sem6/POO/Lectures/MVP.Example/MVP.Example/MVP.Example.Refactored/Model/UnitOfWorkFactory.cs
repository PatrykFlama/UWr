using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MVP.Example.Refactored.Model
{
    public class UnitOfWorkFactory
    {
        private static Func<IUnitOfWork> _provider;

        public static void SetProvider( Func<IUnitOfWork> provider )
        {
            _provider = provider;
        }

        public IUnitOfWork Create()
        {
            return _provider();
        }
    }
}
