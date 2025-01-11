using Data;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Mvc;
using ProjectGame.Helpers;
using System.Security.Claims;

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
            return View();
        }
        [HttpPost]
        public async Task<IActionResult> Login(string name, string password)
        {
            var user = await _authService.Login(name, password);

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
            return View();
        }


        public IActionResult Register()
        {
            return View();
        }

        [HttpPost]
        public async Task<IActionResult> Register(string name, string password)
        {
            bool success = await _authService.Register(name, password);

            if(success)
            {
                return RedirectToAction("Login");
            }

            ModelState.AddModelError("", "Invalid username or password");
            return View();
        }

        public async Task<IActionResult> Logout()
        {
            await HttpContext.SignOutAsync(CookieAuthenticationDefaults.AuthenticationScheme);
            return RedirectToAction("Login");
        }
    }
}
