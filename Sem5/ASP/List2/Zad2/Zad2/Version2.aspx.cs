using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

namespace deklaracja
{
    public partial class WebForm1 : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {
            generateTasks();
        }
        private void generateTasks()
        {
            for(int i = 1; i <= 10; i++)
            {
                Label lbl = new Label();
                lbl.Text = "Zadanie " + i.ToString() + " ";
                TextBox tb = new TextBox();
                tb.ID = "task" + i.ToString();
                tasks.Controls.Add(lbl);
                tasks.Controls.Add(tb);
            }
        }

        protected void Submit_Deklaracja(object sender, EventArgs e)
        {
            string name = this.name.Text;
            string surname = this.surname.Text;
            string date = this.date.Text;
            string list_number = this.list_number.Text;
            string classname = this.name_of_class.Text;
            string[] tasks = new string[10];
            if(name == "")
            {
                Response.Redirect("Version2.aspx");
            }
            if(surname == "")
            {
                Response.Redirect("Version2.aspx");
            }
            if(date == "")
            {
                Response.Redirect("Version2.aspx");
            }
            if(list_number == "")
            {
                Response.Redirect("Version2.aspx");
            }
            for(int i = 1; i <= 10; i++)
            {
                var tb = (TextBox)this.tasks.FindControl("task" + i.ToString());
                if(tb.Text == "")
                {
                    tasks[i - 1] = "0";
                } else
                {
                    tasks[i - 1] = tb.Text;
                }
            }
            string url = "print.aspx?name=" + name
                            + "&surname=" + surname
                            + "&date=" + date
                            + "&course=" + classname
                            + "&list_number=" + list_number;

            Session["tasks"] = tasks;
            Response.Redirect(url);
        }
    }
}