using Microsoft.AspNetCore.Mvc;
using Zad3.Models;

namespace Zad3.Controllers
{
	public class Account : Controller
	{
		public IActionResult Login()
		{
			return View(new LoginViewModel());
		}
	}
}
