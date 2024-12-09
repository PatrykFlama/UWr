using System.Diagnostics;
using Microsoft.AspNetCore.Mvc;
using Zad5.Models;

namespace Zad5.Controllers
{
    public class HomeController : Controller
    {
        public IActionResult Index() 
        { 
            return View(); 
        }
    }
}
