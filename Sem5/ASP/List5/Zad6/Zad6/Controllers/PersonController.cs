using Zad6.Data;
using Zad6.Models;
using Microsoft.AspNetCore.Mvc;
using System;
using System.Threading.Tasks;
using Zad6.Data;

namespace Zad6.Controllers
{
    public class PersonController : Controller
    {
        private readonly IDatabaseConnection _repository;

        public PersonController(IDatabaseConnection repository)
        {
            _repository = repository;
        }

        // GET: Person
        public IActionResult Index()
        {
            var persons = _repository.GetAllPersons();
            return View(persons);
        }

        // GET: Person/Create
        public IActionResult Create()
        {
            return View();
        }

        // POST: Person/Create
        [HttpPost]
        [ValidateAntiForgeryToken]
        public IActionResult Create([Bind("Name,Surname")] PersonModel person)
        {
            if(ModelState.IsValid)
            {
                _repository.AddPerson(person);
                return RedirectToAction(nameof(Index));
            }
            return View(person);
        }

        // GET: Person/Edit/:id
        public IActionResult Edit(int id)
        {
            var person = _repository.GetPersonById(id);
            if(person == null)
            {
                return NotFound();
            }
            return View(person);
        }

        // POST: Person/Edit/:id
        [HttpPost]
        [ValidateAntiForgeryToken]
        public IActionResult Edit(int id, [Bind("ID,Name,Surname")] PersonModel person)
        {
            if(id != person.ID)
            {
                return NotFound();
            }

            if(ModelState.IsValid)
            {
                _repository.UpdatePerson(person);
                return RedirectToAction(nameof(Index));
            }
            return View(person);
        }

        // GET: Person/Delete/:id
        public IActionResult Delete(int id)
        {
            var person = _repository.GetPersonById(id);
            if(person == null)
            {
                return NotFound();
            }
            return View(person);
        }

        // POST: Person/Delete/:id
        [HttpPost, ActionName("Delete")]
        [ValidateAntiForgeryToken]
        public IActionResult DeleteConfirmed(int id)
        {
            _repository.DeletePerson(id);
            return RedirectToAction(nameof(Index));
        }
    }
}
