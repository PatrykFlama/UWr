using Application;
using Domain.Secondary;
using Model;
using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using System.Web.Routing;

namespace Hexagonal.Example
{
    public class MvcApplication : System.Web.HttpApplication
    {
        protected void Application_Start()
        {
            AreaRegistration.RegisterAllAreas();
            RouteConfig.RegisterRoutes(RouteTable.Routes);

            // Composition Root
            PersonRepositoryFactory.SetProvider( context => new PersonRepositoryImpl( context.Context ) );
            Database.SetInitializer<ModelDbContext>( new DropCreateDatabaseIfModelChanges<ModelDbContext>() );
        }
    }
}
