using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Configuration;
using System.Data;
using System.Data.SqlClient;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace MVP.Example
{
    public partial class FrmMain : Form
    {
        public FrmMain()
        {
            InitializeComponent();
            InitializeData();
        }

        void InitializeData()
        {
            lstPerson.Items.Clear();
            lstPerson.Columns.Clear();

            lstPerson.Columns.Add("Name", 60);
            lstPerson.Columns.Add("Surname", -2);

            using (SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["cs"].ConnectionString))
            {
                conn.Open();

                using (SqlCommand cmd = new SqlCommand("select * from Person", conn))
                {
                    var reader = cmd.ExecuteReader();

                    while ( reader.Read() )
                    {
                        var li = lstPerson.Items.Add(reader["Name"].ToString());
                        li.SubItems.Add(reader["Surname"].ToString());
                        li.Tag = (int)reader["ID"];
                    }
                }
            }
        }

        private void button1_Click(object sender, EventArgs e)
        {
            var result = new FrmAddItem().ShowDialog();
            if ( result == DialogResult.OK )
            {
                InitializeData();
            }
        }

        private void lstPerson_SelectedIndexChanged(object sender, EventArgs e)
        {

        }
    }
}
