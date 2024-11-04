using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

namespace Zad4
{
    public partial class PseudoSingleton : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {
            var data = Global.SingletonData;
            lblSingleton.Text = $"{data}";
        }
    }
}