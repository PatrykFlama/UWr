using RepoUoWExample.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RepoUoWExample.Implementation.EntityFramework
{
    public class EFChildRepository : IGenericRepository<IChild>
    {
        ParentChildDbContext _context;

        public EFChildRepository( ParentChildDbContext context )
        {
            this._context = context;
        }

        public IQueryable<IChild> Query
        {
            get
            {
                return this._context.Child;
            }
        }

        public void Delete( IChild t )
        {
            if ( t is Child )
            {
                this._context.Child.Remove( t as Child );
                this._context.SaveChanges();
            }
            else
            {
                throw new ArgumentException();
            }
        }

        public void Insert( IChild t )
        {
            if ( t is Child )
            {
                this._context.Child.Add( t as Child );
                this._context.SaveChanges();
            }
            else
            {
                throw new ArgumentException();
            }
        }

        public IChild New()
        {
            return new Child();
        }

        public void Update( IChild t )
        {
            this._context.SaveChanges();
        }
    }
}
