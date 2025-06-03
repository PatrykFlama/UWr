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

            model.Persons = this.DbContext.Persons;

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
                var person = new Person();
                person.Name = model.Name;
                person.Surname = model.Surname;

                this.DbContext.Persons.Add( person );
                this.DbContext.SaveChanges();

                return RedirectToAction( "Index" );
            }

            return View( model );
        }

        [HttpGet]
        public ActionResult Edit(int? ID)
        {
            var person = this.DbContext.Persons.FirstOrDefault( p => p.ID == ID );
            if ( person != null )
            {
                var model     = new EditModel();
                model.Name    = person.Name;
                model.Surname = person.Surname;

                return View( model );
            }

            return this.HttpNotFound();
        }

        [HttpPost]
        public ActionResult Edit( int? ID, EditModel model )
        {
            if ( this.ModelState.IsValid )
            {
                var person = this.DbContext.Persons.FirstOrDefault( p => p.ID == ID );
                if ( person != null )
                {
                    person.Name    = model.Name;
                    person.Surname = model.Surname;

                    this.DbContext.SaveChanges();

                    return RedirectToAction( "Index" );
                }

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