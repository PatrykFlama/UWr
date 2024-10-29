using System;
using System.Collections.Generic;
using System.Drawing;
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
            GenerateExerciseFields(10);
            displayError("Errors will be displayed here");
        }

        private void GenerateExerciseFields(int n)
        {
            phExercises.Controls.Clear();

            for(int i = 1; i <= n; i++)
            {
                Label lbl = new Label { Text = $"Points for Exercise {i}: " };
                TextBox txt = new TextBox { ID = $"txtPoints{i}", Text = "0", TextMode=TextBoxMode.Number };

                phExercises.Controls.Add(lbl);
                phExercises.Controls.Add(txt);
                phExercises.Controls.Add(new LiteralControl("<br /><br />"));

                //exercises.Controls.Add(lbl);
                //exercises.Controls.Add((TextBox)txt);
                //exercises.Controls.Add(new LiteralControl("<br /><br />"));
            }
        }

        private void displayError(string s)
        {
            phError.Controls.Clear();
            Label lbl = new Label { Text = s };
            phError.Controls.Add(lbl);
        }

        protected void btnCalculate_Click(object sender, EventArgs e)
        {
            int totalPoints = 0;

            for(int i = 1; i <= 10; i++)
            {
                TextBox txtPoints = (TextBox)phExercises.FindControl($"txtPoints{i}");
                //TextBox txtPoints = (TextBox)this.exercises.FindControl($"txtPoints{i}");
                int val = int.Parse(txtPoints.Text);
                totalPoints += val;
            }

            lblTotalPoints.Text = totalPoints.ToString();
        }

        protected void btnSubmit_Click(object sender, EventArgs e)
        {
            if(txtName.Text == "" || txtDate.Text == "")
            {
                displayError("Name and date cant be empty!");
                return;
            }

            string url = "print.aspx" +
                    "?name=" + txtName.Text +
                    "&date=" + txtDate.Text +
                    "&course=" + txtCourseName.Text +
                    "&list=" + txtListNumber.Text +
                    "&sum=" + lblTotalPoints.Text;

            string[] exercises = new String[10];
            for(int i = 1; i <= 10; i++) {
                TextBox txtPoints = (TextBox)phExercises.FindControl($"txtPoints{i}");
                exercises[i-1] = txtPoints.Text;
            }

            Session["exercises"] = exercises;

            Response.Redirect(url);
        }
    }
}