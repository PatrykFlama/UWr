using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using Zad1Framework.CustomClass;

namespace Zad1Framework.Controllers
{
        public class PageController : Controller
        {
            public ActionResult Render()
            {
                var routeData = this.Request.RequestContext.RouteData.Values;
                string site = routeData[CMSCustomRoute.SITENAME] as string;
                string page = routeData[CMSCustomRoute.PAGENAME] as string;
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