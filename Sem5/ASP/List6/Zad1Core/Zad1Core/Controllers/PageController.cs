using Microsoft.AspNetCore.Mvc;
using System.Diagnostics;
using Zad1Core.CusomClasses;
using Zad1Core.Models;

namespace Zad1Core.Controllers
{
    public class PageController : Controller
    {
        public IActionResult Render()
        {
            var routeData = this.Request.RouteValues;
            string site = routeData[CMSCustomRouteTransformer.SITENAME] as string;
            string page = routeData[CMSCustomRouteTransformer.PAGENAME] as string;

            // odczyt z magazynu danych
            // renderowanie
            var model = new PageRenderModel()
            {
                Site = site,
                Page = page
            };
            return View(model);
        }
    }

    public class PageRenderModel
    {
        public string Site { get; set; }
        public string Page { get; set; }
    }
}
