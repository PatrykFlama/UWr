using Data;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Mvc;
using ProjectGame.Helpers;
using System.Security.Claims;
using ProjectGame.Models;
using Microsoft.Identity.Client;
using Microsoft.AspNetCore.Authorization;

namespace ProjectGame.Controllers
{
    public class AccountController : Controller
    {
        private readonly AppDbContext _context;
        private readonly AuthService _authService;
        private readonly GamesHistoryService _historyService;

        public AccountController(AppDbContext context, AuthService authService, GamesHistoryService historyService)
        {
            _context = context;
            _authService = authService;
            _historyService = historyService;
        }

        public IActionResult Login()
        {
            var model = new LoginViewModel();
            return View(model);
        }
        [HttpPost]
        public async Task<IActionResult> Login(LoginViewModel model)
        {
            if(!ModelState.IsValid)
            {
                return View(model);
            }

            var user = await _authService.Login(model.Name, model.Password);

            if(user != null)
            {
                // Tworzenie claims
                var claims = new List<Claim>
                {
                    new Claim(ClaimTypes.Name, user.Name),   // Dodanie nazwy użytkownika
                };

                var identity = new ClaimsIdentity(claims, CookieAuthenticationDefaults.AuthenticationScheme);
                var principal = new ClaimsPrincipal(identity);

                // Zalogowanie użytkownika
                await HttpContext.SignInAsync(CookieAuthenticationDefaults.AuthenticationScheme, principal);

                return Redirect("/");
            }

            ModelState.AddModelError("Errors", "Invalid login");
            return View();
        }


        public IActionResult Register()
        {
            var model = new RegisterViewModel();
            return View(model);
        }

        [HttpPost]
        public async Task<IActionResult> Register(RegisterViewModel model)
        {
            if(!ModelState.IsValid)
            { 
                return View(model); 
            }

            bool success = await _authService.Register(model.Name, model.Password);

            if(success)
            {
                return RedirectToAction("Login");
            }

            ModelState.AddModelError("Name", "Username taken");
            return View();
        }

        [Authorize]
        public async Task<IActionResult> Logout()
        {
            await HttpContext.SignOutAsync(CookieAuthenticationDefaults.AuthenticationScheme);
            return Redirect("/");
        }

        [Authorize]
        [HttpGet]
        public async Task<IActionResult> GamesHistory()
        {
            GamesHistoryViewModel model = new GamesHistoryViewModel();

            if(ModelState.IsValid && User.Identity.IsAuthenticated)
            {
                model.history = await _historyService.GetGamesByPlayer(User.Identity.Name);
            }

            return View(model);
        }
    }
}
