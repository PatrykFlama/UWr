using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Mvc;
using System.Security.Claims;
using Zad2.Helpers;
using Microsoft.EntityFrameworkCore;
using Zad2.Data;

namespace Zad2.Controllers
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

        [HttpGet]
        public IActionResult Register()
        {
            var roles = _context.Roles.Select(r => r.RoleName).ToList();
            ViewBag.Roles = roles;
            return View();
        }

        [HttpPost]
        public async Task<IActionResult> Register(string userName, string email, string password, string role)
        {
            await _authService.Register(userName, email, password, role);
            return RedirectToAction("Login");
        }


        [HttpGet]
        public IActionResult Login()
        {
            return View();
        }

        [HttpPost]
        public async Task<IActionResult> Login(string email, string password)
        {
            var user = await _authService.Login(email, password);

            if(user != null)
            {
                // Pobranie roli użytkownika
                var userRole = await _authService.GetUserRole(user.Id); // Załóżmy, że masz metodę GetUserRole w AuthService

                // Tworzenie claims
                var claims = new List<Claim>
                {
                    new Claim(ClaimTypes.Email, email),
                    new Claim(ClaimTypes.Name, user.UserName),   // Dodanie nazwy użytkownika
                    new Claim(ClaimTypes.Role, userRole)         // Dodanie roli
                };

                var identity = new ClaimsIdentity(claims, CookieAuthenticationDefaults.AuthenticationScheme);
                var principal = new ClaimsPrincipal(identity);

                // Zalogowanie użytkownika
                await HttpContext.SignInAsync(CookieAuthenticationDefaults.AuthenticationScheme, principal);

                return RedirectToAction("Index", "Home");
            }

            ModelState.AddModelError("", "Invalid login");
            return View();
        }


        public async Task<IActionResult> Logout()
        {
            await HttpContext.SignOutAsync(CookieAuthenticationDefaults.AuthenticationScheme);
            return RedirectToAction("Login");
        }
    }

}
