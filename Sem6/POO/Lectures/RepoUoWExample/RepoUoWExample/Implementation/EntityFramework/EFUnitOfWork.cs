using RepoUoWExample.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RepoUoWExample.Implementation.EntityFramework
{
    public class EFUnitOfWork : IUnitOfWork
    {
        ParentChildDbContext _context;

        public EFUnitOfWork( ParentChildDbContext context )
        {
            this._context = context;
        }

        public IGenericRepository<IParent> Parent { get => new EFParentRepository( this._context ); }
        public IGenericRepository<IChild> Child { get => new EFChildRepository( this._context ); }
    }
}
