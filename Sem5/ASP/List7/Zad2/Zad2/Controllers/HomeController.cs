using System.Diagnostics;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Zad2.Models;

namespace Zad2.Controllers
{
    public class HomeController : Controller
    {
        public IActionResult Index()
        {
            return View();
        }

        [Authorize]
        public IActionResult Restricted()
        {
            return View(); // Widok dostêpny tylko dla zalogowanych
        }

        [Authorize(Roles = "Admin")]
        public IActionResult Admin()
        {
            //return Content("Dostêp tylko dla administratorów!");
            return View();
        }

        [Authorize(Roles = "User")]
        public IActionResult User()
        {
            //return Content("Dostêp tylko dla u¿ytkowników!");
            return View();
        }

        [Authorize(Policy = "AdminOnly")]
        public IActionResult AdminPolicy()
        {
            //return Content("Dostêp na podstawie polityki dla administratorów!");
            return View();
        }

    }
}
