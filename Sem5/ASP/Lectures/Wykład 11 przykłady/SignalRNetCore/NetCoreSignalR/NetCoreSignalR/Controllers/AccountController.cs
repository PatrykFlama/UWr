using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Mvc;
using NetCoreSignalR.Models;
using System.Security.Claims;

namespace NetCoreSignalR.Controllers
{
    public class AccountController : Controller
    {
        [HttpGet]
        public IActionResult Logon()
        {
            var model = new AccountLogonModel();
            return View(model);
        }

        [HttpPost]
        public async Task<IActionResult> Logon(AccountLogonModel model)
        {
            if ( this.ModelState.IsValid )
            {
                if ( model.UserName == model.Password )
                {
                    List<Claim> claims = new List<Claim>
                    {
                        new Claim(ClaimTypes.Name, model.UserName)
                    };

                    // create identity
                    ClaimsIdentity identity = new ClaimsIdentity(claims, CookieAuthenticationDefaults.AuthenticationScheme);
                    ClaimsPrincipal principal = new ClaimsPrincipal(identity);

                    await this.HttpContext.SignInAsync(CookieAuthenticationDefaults.AuthenticationScheme, principal);

                    return Redirect("/");
                }
            }

            return View(model);
        }

    }
}
