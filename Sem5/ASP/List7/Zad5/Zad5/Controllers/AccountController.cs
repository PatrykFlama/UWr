using Microsoft.AspNetCore.Mvc;
using Zad5.Data;
using OtpNet;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Authentication;
using Microsoft.EntityFrameworkCore;
using System.Security.Claims;
using Microsoft.AspNetCore.Authorization;

namespace Zad5.Controllers
{
    public class AccountController : Controller
    {
        private readonly AppDbContext _dbContext;

        public AccountController(AppDbContext dbContext)
        {
            _dbContext = dbContext;
        }



        [HttpGet]
        public IActionResult Register()
        {
            return View();
        }

        [HttpPost]
        public async Task<IActionResult> Register(string username)
        {
            var userTOTP = new UserTOTP
            {
                UserName = username,
            };

            await _dbContext.UsersTOTP.AddAsync(userTOTP);
            await _dbContext.SaveChangesAsync();

            return RedirectToAction("Login");
        }


        [HttpGet]
        public IActionResult Login()
        {
            return View();
        }

        [HttpPost]
        public async Task<IActionResult> Login(string username)
        {
            var user = await _dbContext.UsersTOTP.SingleOrDefaultAsync(u => u.UserName == username);

            if(user != null)
            {
                var claims = new List<Claim>
                {
                    new Claim(ClaimTypes.NameIdentifier, user.Id.ToString()),
                    new Claim(ClaimTypes.Name, user.UserName),   // Dodanie nazwy użytkownika
                };

                var identity = new ClaimsIdentity(claims, CookieAuthenticationDefaults.AuthenticationScheme);
                var principal = new ClaimsPrincipal(identity);

                await HttpContext.SignInAsync(CookieAuthenticationDefaults.AuthenticationScheme, principal);

                return RedirectToAction("Index", "Home");
            }

            ModelState.AddModelError("", "Invalid username");
            return View();
        }

        [Authorize]
        [HttpGet]
        public IActionResult Enable2FA()
        {
            return View();
        }

        [Authorize]
        [HttpGet]
        public IActionResult DisplayTOTPKey() 
        { 
            return View(); 
        }

        [Authorize]
        [HttpPost]
        public IActionResult Enable2FA(int userId)
        {
            var id = int.Parse(User.FindFirst(ClaimTypes.NameIdentifier)?.Value);
            //var user = _dbContext.UsersTOTP.SingleOrDefaultAsync(u => u.Id == id);
            //if(user == null) return NotFound();

            var key = KeyGeneration.GenerateRandomKey(20);
            var base32Key = Base32Encoding.ToString(key);

            var totpKey = new UserTOTPKey
            {
                UserTOTPID = id,
                TOTPKey = base32Key,
                CreatedAt = DateTime.UtcNow
            };

            //_dbContext.UserTOTPKeys.Add(totpKey);
            //_dbContext.SaveChanges();

            ViewBag.TOTPKey = base32Key;

            HttpContext.Response.Cookies.Append("TOTPKey", base32Key, new CookieOptions
            {
                //HttpOnly = true,
                //Secure = true,
                SameSite = SameSiteMode.Strict,
                Expires = DateTimeOffset.UtcNow.AddDays(7) // Ustaw długość ważności ciasteczka
            });

            return View("DisplayTOTPKey");
        }

        [HttpGet]
        [Authorize]
        public IActionResult Verify2FA()
        {
            return View();
        }

        [Authorize]
        [HttpPost]
        public IActionResult Verify2FA(int userId, string code)
        {
            //var id = int.Parse(User.FindFirst(ClaimTypes.NameIdentifier)?.Value);
            //var totpKey = _dbContext.UserTOTPKeys
            //    .FirstOrDefault(k => k.UserTOTPID == id);

            //if(totpKey == null) return Unauthorized();

            if(!HttpContext.Request.Cookies.TryGetValue("TOTPKey", out var base32Key))
            {
                return Unauthorized("Brak klucza");
            }

            var totp = new Totp(Base32Encoding.ToBytes(base32Key));
            bool isValid = totp.VerifyTotp(code, out _, VerificationWindow.RfcSpecifiedNetworkDelay);

            //var totp = new Totp(Base32Encoding.ToBytes(totpKey.TOTPKey));
            //bool isValid = totp.VerifyTotp(code, out _, VerificationWindow.RfcSpecifiedNetworkDelay);

            if(isValid)
            {
                return Content("Kod poprawany");
            }

            ModelState.AddModelError("", "Kod niepoprawny");
            return View();
        }

        public async Task<IActionResult> Logout()
        {
            HttpContext.Response.Cookies.Delete("TOTPKey");
            await HttpContext.SignOutAsync(CookieAuthenticationDefaults.AuthenticationScheme);
            return RedirectToAction("Login");
        }
    }

}
