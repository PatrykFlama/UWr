using Microsoft.AspNetCore.Mvc;

namespace ProjectGame.Controllers
{
    public class AccountController : Controller
    {
        public IActionResult Index()
        {
            return View();
        }
    }
}
