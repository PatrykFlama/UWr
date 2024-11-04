using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

namespace Zad4
{
    public partial class WebForm1 : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {
            Application.Lock();
            Application["GlobalCounter"] = (int)Application["GlobalCounter"] + 1;
            Application.UnLock();

            lblGlobalCounter.Text = $"Global Counter: {Application["GlobalCounter"]}";

            Items["Time"] = DateTime.Now;

            if(Session["SessionInfo"] == null)
                Session["SessionInfo"] = DateTime.Now.ToString();

            var info = Session["SessionInfo"] as string ?? "no info";
            lblSessionInfo.Text = $"Session info: {info}";
        }
    }
}