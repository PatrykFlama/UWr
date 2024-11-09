using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Routing;
using System.Web.Security;
using System.Web.SessionState;

namespace Zad1
{
    public class Global : System.Web.HttpApplication
    {
        protected void Application_Start(object sender, EventArgs e)
        {
            RouteTable.Routes.Add(
                "customroute",
                new CustomRouter(
                    new RouteValueDictionary(new { tenant = "detault" }),
                    new CustomRouteHandler()
                )
            );
        }
    }


    public class CustomRouteHandler : IRouteHandler
    {
        #region IRouteHandler Members
        public IHttpHandler GetHttpHandler(RequestContext requestContext)
        {
            return new CustomHttpHandler();
        }
        #endregion
    }

    public class CustomHttpHandler : IHttpHandler
    {
        #region IHttpHandler Members
        public bool IsReusable
        {
            get { return true; }
        }
        public void ProcessRequest(HttpContext context)
        {
            var routeData = context.Request.RequestContext.RouteData.Values;
            string response = (string.Format("tenant: {0} site: {1} page: {2}", routeData["tenant"], routeData["siteName"], routeData["pageName"]));

            context.Response.Write(response);
        }
        #endregion
    }
}