using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RepoUoWExample.Model
{
    public interface IUnitOfWork
    {
        IGenericRepository<IParent> Parent { get; }
        IGenericRepository<IChild> Child { get; }
    }

}
