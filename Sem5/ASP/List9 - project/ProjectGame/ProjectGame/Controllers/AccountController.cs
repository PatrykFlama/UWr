using Data;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Mvc;
using ProjectGame.Helpers;
using System.Security.Claims;
using ProjectGame.Models;

namespace ProjectGame.Controllers
{
    public class AccountController : Controller
    {
        private readonly AppDbContext _context;
        private readonly AuthService _authService;

        public AccountController(AppDbContext context, AuthService authService)
        {
            _context = context;
            _authService = authService;
        }

        public IActionResult Login()
        {
            var model = new LoginViewModel();
            return View(model);
        }
        [HttpPost]
        public async Task<IActionResult> Login(LoginViewModel model)
        {
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

            ModelState.AddModelError("", "Invalid login");
            model.Errors.Add("Invalid Login");
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
            bool success = await _authService.Register(model.Name, model.Password);

            if(success)
            {
                return RedirectToAction("Login");
            }

            ModelState.AddModelError("", "Invalid username or password");
            model.Errors.Add("Invalid username or password");
            return View();
        }

        public async Task<IActionResult> Logout()
        {
            await HttpContext.SignOutAsync(CookieAuthenticationDefaults.AuthenticationScheme);
            return Redirect("/");
        }
    }
}
