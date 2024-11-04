using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Security;
using System.Web.SessionState;
using static System.Net.Mime.MediaTypeNames;

namespace Zad4
{
    public class Global : System.Web.HttpApplication
    {
        const string SINGL = "singleton_data";

        public static string SingletonData
        {
            get
            {
                if(HttpContext.Current.Application[SINGL] == null)
                {
                    lock (HttpContext.Current.Application) {
                    // here we can create object once and save it to serve it
                        HttpContext.Current.Application[SINGL] = DateTime.Now.ToString();
                    }
                }

                return (string)HttpContext.Current.Application[SINGL];
            }
        }

        protected void Application_Start(object sender, EventArgs e)
        {
            Application["GlobalCounter"] = 0;

        }
        public static void IncrementGlobalCounter() 
        {
            lock(HttpContext.Current.Application) { // lock resource to ensure thread safety
                HttpContext.Current.Application["GlobalCounter"] = 
                    (int)HttpContext.Current.Application["GlobalCounter"] + 1;
            }
        }
    }
}