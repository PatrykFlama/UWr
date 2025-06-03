using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Model
{
    public class ModelDbContext : DbContext
    {
        public ModelDbContext() : base() { }

        public ModelDbContext( string cs ) : base( cs ) { }

        public virtual IDbSet<Person> Persons { get; set; }
    }
}
