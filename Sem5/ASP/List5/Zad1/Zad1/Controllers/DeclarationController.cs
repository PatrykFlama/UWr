using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using Zad1.Models;

namespace Zad1.Controllers
{
    public class DeclarationController : Controller
    {
        [HttpGet]
        public ActionResult Input()
        {
            DeclarationModel model = new DeclarationModel();
            model.Points = new int?[10];
            return View(model);
        }

        [HttpPost]
        public ActionResult Input(DeclarationModel model)
        {
            if(ModelState.IsValid)
            {
                for(int i = 0; i < model.Points.Length; i++)
                {
                    if(model.Points[i] == null) model.Points[i] = 0;
                }

                Session["DeclarationModel"] = model;
                return Redirect("Print");
            }

            return View(model);
        }

        public ActionResult Print()
        {
            var model = Session["DeclarationModel"] as DeclarationModel;
            
            if(model == null)
            {
                return RedirectToAction("Input");
            }

            return View(model);
        }
    }
}