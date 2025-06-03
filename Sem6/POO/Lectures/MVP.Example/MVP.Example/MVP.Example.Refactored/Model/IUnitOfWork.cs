using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MVP.Example.Refactored.Model
{
    public interface IUnitOfWork : IDisposable
    {
        IPersonRepository Person { get; }
        void Dispose();
    }
}
