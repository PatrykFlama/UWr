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
            return View();
        }

        [Authorize(Roles = "Admin")]
        public IActionResult Admin()
        {
            //return Content("Dostęp tylko dla administratorów!");
            return View();
        }

        [Authorize(Roles = "User")]
        public IActionResult User()
        {
            //return Content("Dostęp tylko dla użytkowników!");
            return View();
        }

        [Authorize(Policy = "AdminOnly")]
        public IActionResult AdminPolicy()
        {
            //return Content("Dostęp na podstawie polityki dla administratorów!");
            return View();
        }

    }
}
