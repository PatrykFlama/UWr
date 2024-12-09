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
            return View(); // Widok dost�pny tylko dla zalogowanych
        }

        [Authorize(Roles = "Admin")]
        public IActionResult Admin()
        {
            //return Content("Dost�p tylko dla administrator�w!");
            return View();
        }

        [Authorize(Roles = "User")]
        public IActionResult User()
        {
            //return Content("Dost�p tylko dla u�ytkownik�w!");
            return View();
        }

        [Authorize(Policy = "AdminOnly")]
        public IActionResult AdminPolicy()
        {
            //return Content("Dost�p na podstawie polityki dla administrator�w!");
            return View();
        }

    }
}
