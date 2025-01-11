using Microsoft.AspNetCore.Mvc;

namespace ProjectGame.Controllers
{
    public class GameController : Controller
    {
        public IActionResult Index()
        {
            return View();
        }

        public IActionResult ListGames()
        {
            return View();
        }

        [HttpPost]
        public IActionResult ListGames(string playerName)
        {
            return View();
        }

        [HttpGet]
        public IActionResult Play(string gameId)
        {
            ViewData["GameId"] = gameId;
            return View();
        }
    }
}
