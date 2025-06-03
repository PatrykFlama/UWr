using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RepoUoWExample.Implementation.EntityFramework
{
    public class ParentChildDbContext : DbContext
    {
        public ParentChildDbContext() { }

        public ParentChildDbContext( string connectionString ) : base( connectionString ) { }

        public IDbSet<Child> Child { get; set; }
        public IDbSet<Parent> Parent { get; set; }

        protected override void OnModelCreating( DbModelBuilder modelBuilder )
        {
            modelBuilder.Entity<Child>()
                .HasRequired( e => e.Parent )
                .WithMany( e => e.Children )
                .HasForeignKey( e => e.ID_PARENT );
        }
    }
}
