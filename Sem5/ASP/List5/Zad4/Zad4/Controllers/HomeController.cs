using Zad4.Models;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Rendering;
using System.Collections.Generic;
using Zad4.Models;

namespace Zad4.Controllers
{
    public class HomeController : Controller
    {
        public IActionResult Index()
        {
            var model = new User
            {
                CountryList = new List<SelectListItem>
                {
                    new SelectListItem { Text = "Polska", Value = "1" },
                    new SelectListItem { Text = "POLSKA", Value = "2" },
                    new SelectListItem { Text = "polska", Value = "3" }
                }
            };

            return View(model);
        }

        [HttpPost]
        public IActionResult Submit(User model)
        {
            if(ModelState.IsValid)
            {
                ViewBag.Message = "Form submitted";
            }
            return View("Index", model);
        }
    }
}
