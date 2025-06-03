using RepoUoWExample.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RepoUoWExample.Implementation.EntityFramework
{
    public class EFParentRepository : IGenericRepository<IParent>
    {
        ParentChildDbContext _context;

        public EFParentRepository( ParentChildDbContext context )
        {
            this._context = context;
        }

        public IQueryable<IParent> Query 
        { 
            get
            {
                return this._context.Parent;
            }
        }

        public void Delete( IParent t )
        {
            if ( t is Parent )
            {
                this._context.Parent.Remove( t as Parent );
                this._context.SaveChanges();
            }
            else
            {
                throw new ArgumentException();
            }
        }

        public void Insert( IParent t )
        {
            if ( t is Parent )
            {
                this._context.Parent.Add( t as Parent );
                this._context.SaveChanges();
            }
            else
            {
                throw new ArgumentException();
            }
        }

        public IParent New()
        {
            return new Parent();
        }

        public void Update( IParent t )
        {
            this._context.SaveChanges();
        }
    }
}
