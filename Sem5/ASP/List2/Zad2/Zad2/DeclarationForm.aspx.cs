using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

namespace Zad2
{
    public partial class WebForm1 : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {
            if(!IsPostBack)
            {
                GenerateExerciseFields(1);
            }
        }

        protected void numExercises_SelectedIndexChanged(object sender, EventArgs e)
        {
            int n = int.Parse(numExercises.Text);
            GenerateExerciseFields(n);
        }

        private void GenerateExerciseFields(int n)
        {
            //phExercises.Controls.Clear();

            for(int i = 1; i <= n; i++)
            {
                Label lbl = new Label { Text = $"Points for Exercise {i}: " };
                TextBox txt = new TextBox { ID = $"txtPoints{i}", Text = "0", TextMode=TextBoxMode.Number };

                //phExercises.Controls.Add(lbl);
                //phExercises.Controls.Add(txt);
                //phExercises.Controls.Add(new LiteralControl("<br /><br />"));

                exercises.Controls.Add(lbl);
                exercises.Controls.Add((TextBox)txt);
                exercises.Controls.Add(new LiteralControl("<br /><br />"));
            }
        }

        protected void btnCalculate_Click(object sender, EventArgs e)
        {
            int totalPoints = 0;
            int n = int.Parse(numExercises.Text);

            for(int i = 1; i <= n; i++)
            {
                //TextBox txtPoints = (TextBox)phExercises.FindControl($"txtPoints{i}");
                TextBox txtPoints = (TextBox)this.exercises.FindControl($"txtPoints{i}");
                int val = int.Parse(txtPoints.Text);
                totalPoints += val;
            }

            lblTotalPoints.Text = totalPoints.ToString();
        }
    }
}