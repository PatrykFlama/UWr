using Microsoft.AspNetCore.Mvc;
using Zad2.Models;

namespace Zad2.Controllers
{
    public class DeclarationController : Controller
    {
        [HttpGet]
        public IActionResult Input()
        {
            DeclarationModel model = new DeclarationModel();
            model.Points = new int?[10];
            return View(model);
        }

        [HttpPost]
        public IActionResult Input(DeclarationModel model)
        {
            if(ModelState.IsValid)
            {
                for(int i = 0; i < model.Points.Length; i++)
                {
                    if(model.Points[i] == null) model.Points[i] = 0;
                }

                ViewBag.m = model;
                return View("Print", model);
            }

            return View(model);
        }

        public IActionResult Print(DeclarationModel model)
        {
            if(model == null)
            {
                return RedirectToAction("Input");
            }

            return View(model);
        }
    }
}
