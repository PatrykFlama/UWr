using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

namespace Zad4Framework
{
    public partial class WebForm1 : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {
            string appSettingValue = ConfigurationManager.AppSettings["AppSettingsKey"];
            string connectionString = ConfigurationManager.ConnectionStrings["MyDatabase"].ConnectionString;
            

            lblLabel.Text = appSettingValue + " " + connectionString;
        }
    }
}