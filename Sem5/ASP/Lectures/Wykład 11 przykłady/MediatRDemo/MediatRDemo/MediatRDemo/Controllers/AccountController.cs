using Domain.Logon;
using MediatR;
using MediatRDemo.Models.Account;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Mvc;
using System.Security.Claims;

namespace MediatRDemo.Controllers
{
    public class AccountController : Controller
    {
        private IMediator _mediator;

        public AccountController( IMediator mediator )
        {
            this._mediator = mediator;
        }


        [HttpGet]
        public IActionResult Logon()
        {
            var model = new LogonModel();
            return View(model);
        }

        [HttpPost]
        public async Task<IActionResult> Logon( LogonModel model)
        {
            if ( this.ModelState.IsValid )
            {
                var logonRequest = new LogonUseCaseParameters()
                {
                    Username = "foo",
                    Password = "foo"
                };

                var logonResult = await this._mediator.Send( logonRequest );

                if ( logonResult.LogonStatus )
                {
                    await _mediator.Publish( new LogonNotification() { Username = model.Username } );

                    List<Claim> claims = new List<Claim>
                    {
                        new Claim(ClaimTypes.Name, model.Username),
                        new Claim(ClaimTypes.Role, model.Username),
                        new Claim(ClaimTypes.DateOfBirth, new DateTime(1990, 1, 1).ToString())
                    };

                    // create identity
                    ClaimsIdentity identity   = new ClaimsIdentity(claims, CookieAuthenticationDefaults.AuthenticationScheme);
                    ClaimsPrincipal principal = new ClaimsPrincipal(identity);

                    await this.HttpContext.SignInAsync( CookieAuthenticationDefaults.AuthenticationScheme, principal );

                    return Redirect( "/" );
                }
            }

            return View(model);
        }
    }
}
