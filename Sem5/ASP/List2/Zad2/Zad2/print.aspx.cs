using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

namespace Zad2
{
    public partial class print : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {
            if(!IsPostBack)
            {
                string imie = Request.QueryString["name"] ?? "Nieznane";
                string data = Request.QueryString["date"] ?? DateTime.Now.ToString("yyyy-MM-dd");
                string kurs = Request.QueryString["course"] ?? "Brak";
                string lista = Request.QueryString["list"] ?? "0";
                string suma = Request.QueryString["sum"] ?? "0";

                string[] punkty = Session["exercises"] as string[] ?? new string[10];

                DataTable table = new DataTable();
                table.Columns.Add("Imie", typeof(string));
                table.Columns.Add("Data", typeof(string));
                table.Columns.Add("Kurs", typeof(string));
                table.Columns.Add("Lista", typeof(string));
                table.Columns.Add("Suma", typeof(string));

                for(int i = 1; i <= 10; i++)
                {
                    table.Columns.Add("Punkty" + i, typeof(string));
                }

                DataRow row = table.NewRow();
                row["Imie"] = imie;
                row["Data"] = data;
                row["Kurs"] = kurs;
                row["Lista"] = lista;
                row["Suma"] = suma;

                for(int i = 0; i < 10; i++)
                {
                    row["Punkty" + (i + 1)] = (i < punkty.Length) ? punkty[i] : "0";
                }

                table.Rows.Add(row);

                GridView1.DataSource = table;
                GridView1.DataBind();
            }
        }
    }
}