using RepoUoWExample.Implementation.EntityFramework;
using RepoUoWExample.Model;
using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RepoUoWExample
{
    internal class Program
    {
        static void Main( string[] args )
        {
            // composition root
            Database.SetInitializer( new DropCreateDatabaseIfModelChanges<ParentChildDbContext>() );
            UnitOfWorkFactory.SetProvider( () => new EFUnitOfWork( new ParentChildDbContext( "server=.\\sql2019;database=ParentChildExample2023;integrated security=true" ) ) );

            // kod kliencki
            var uow = new UnitOfWorkFactory().Create();

            // dodanie elementu
            var parent        = uow.Parent.New();
            parent.ParentName = "nowy parent";
            uow.Parent.Insert( parent );

            // zapytanie
            foreach ( var child in uow.Parent.Query )
            {
                Console.WriteLine( child.ParentName );
            }

            Console.ReadLine();
        }
    }
}
