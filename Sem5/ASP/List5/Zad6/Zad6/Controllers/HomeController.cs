using Microsoft.AspNetCore.Mvc;

namespace Zad6.Controllers
{
    public class HomeController : Controller
    {
        public IActionResult Index()
        {
            Response.Redirect("Person");
            return View();
        }
    }
}
