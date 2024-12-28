using Microsoft.AspNetCore.Mvc;

namespace ProjectGame.Controllers
{
    public class GameController : Controller
    {
        public IActionResult Index()
        {
            return View();
        }

        [HttpGet]
        public IActionResult Play(string gameId)
        {
            if(string.IsNullOrEmpty(gameId))
            {
                gameId = Guid.NewGuid().ToString("N").Substring(0, 6);
            }

            ViewData["GameId"] = gameId;
            return View();
        }
    }
}
