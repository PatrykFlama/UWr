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
    public partial class FrmAddItem : Form
    {
        public FrmAddItem()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            var name = this.txtName.Text;
            var surname = this.txtSurname.Text;

            if ( !string.IsNullOrEmpty( name ) &&
                 !string.IsNullOrEmpty( surname ) )
            {
                using (SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["cs"].ConnectionString))
                {
                    conn.Open();

                    using (SqlCommand cmd = new SqlCommand("insert into Person (Name, Surname) values (@Name, @Surname)", conn))
                    {
                        cmd.Parameters.AddWithValue("@Name", name);
                        cmd.Parameters.AddWithValue("@Surname", surname);

                        try
                        {
                            cmd.ExecuteNonQuery();

                            this.DialogResult = DialogResult.OK;
                            this.Close();
                        }
                        catch ( Exception ex )
                        {
                            MessageBox.Show(ex.Message);
                        }
                    }
                }
            }
        }

        private void button2_Click(object sender, EventArgs e)
        {
            this.DialogResult = DialogResult.Cancel;
            this.Close();
        }      
    }
}
