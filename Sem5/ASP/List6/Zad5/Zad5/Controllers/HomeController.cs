using Microsoft.AspNetCore.Mvc;
using System.Diagnostics;
using Zad5.Models;

namespace Zad5.Controllers
{
    public class HomeController : Controller
    {
        public IActionResult Index()
        {
            return View(new UserModel());
        }

        [HttpPost]
        public IActionResult Index(UserModel model)
        {
            return View(model);
        }
    }
}
