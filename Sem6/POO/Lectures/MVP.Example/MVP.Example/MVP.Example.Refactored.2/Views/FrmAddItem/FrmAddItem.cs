using MVP.Example.Refactored.Model;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace MVP.Example.Refactored
{
    public partial class FrmAddItem : Form, IFrmAddItem
    {
        public FrmAddItem()
        {
            InitializeComponent();
        }

        public FrmAddItemPresenter Presenter { get; set; }

        public void ShowModal()
        {
            this.ShowDialog();
        }


        private void button1_Click(object sender, EventArgs e)
        {
            var name = this.txtName.Text;
            var surname = this.txtSurname.Text;

            if ( !string.IsNullOrEmpty(name) &&
                 !string.IsNullOrEmpty(surname))
            {
                Person person = new Person();
                person.Name = name;
                person.Surname = surname;

                try
                {
                    this.Presenter.AddNewPerson(person);

                    this.Close();
                }
                catch ( Exception ex )
                {
                    MessageBox.Show(ex.Message);
                }
            }
        }

        private void button2_Click(object sender, EventArgs e)
        {
            this.Close();
        }
    }
}
