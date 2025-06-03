using Domain;
using Domain.Primary;
using Hexagonal.Example.Models;
using Model;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Web;
using System.Web.Mvc;

namespace Hexagonal.Example.Controllers
{
    public class HomeController : BaseController
    {        
        public ActionResult Index()
        {
            var model = new IndexModel();

            var useCase            = new PersonListUseCase();
            var useCaseParameters  = new PersonListUseCaseParameters();
            var applicationContext = new ApplicationContext()
            {
                Context = this.DbContext
            };

            model.Persons = useCase.Execute( useCaseParameters, applicationContext ).Persons;

            return View(model);
        }

        [HttpGet]
        public ActionResult Create()
        {
            var model = new CreateModel();
            return View( model );
        }

        [HttpPost]
        public ActionResult Create(CreateModel model)
        {
            if ( this.ModelState.IsValid )
            {
                var useCase            = new PersonAddUseCase();
                var useCaseParameters  = new PersonAddUseCaseParameters()
                {
                    Name = model.Name,
                    Surname = model.Surname
                };
                var applicationContext = new ApplicationContext()
                {
                    Context = this.DbContext
                };
                useCase.Execute( useCaseParameters, applicationContext );

                return RedirectToAction( "Index" );
            }

            return View( model );
        }

        [HttpGet]
        public ActionResult Edit(int? ID)
        {
            var useCase            = new PersonOneUseCase();
            var useCaseParameters  = new PersonOneUseCaseParameters()
            {
                ID = ID.Value
            };
            var applicationContext = new ApplicationContext()
            {
                Context = this.DbContext
            };
            var useCaseResult = useCase.Execute( useCaseParameters, applicationContext );


            if ( useCaseResult != null && useCaseResult.Person != null )
            {
                var model     = new EditModel();
                model.Name    = useCaseResult.Person.Name;
                model.Surname = useCaseResult.Person.Surname;

                return View( model );
            }

            return this.HttpNotFound();
        }

        [HttpPost]
        public ActionResult Edit( int? ID, EditModel model )
        {
            if ( this.ModelState.IsValid )
            {
                var useCase            = new PersonModifyUseCase();
                var useCaseParameters  = new PersonModifyUseCaseParameters()
                {
                    ID      = ID.Value,
                    Name    = model.Name,
                    Surname = model.Surname
                };
                var applicationContext = new ApplicationContext()
                {
                    Context = this.DbContext
                };
                useCase.Execute( useCaseParameters, applicationContext );

                return RedirectToAction( "Index" );
            }

            return View( model );
        }
    }

    public abstract class BaseController : Controller, IDisposable
    {
        private ModelDbContext _context;
        public ModelDbContext DbContext
        {
            get
            {
                if ( _context == null )
                {
                    _context = new ModelDbContext( ConfigurationManager.AppSettings["databaseConnectionString"] );
                }

                return _context;
            }
        }

        void IDisposable.Dispose()
        {
            if ( _context != null )
            {
                _context.Dispose();
            }
        }
    }
}