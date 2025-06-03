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
    public partial class FrmMain : Form, IFrmMain
    {
        public FrmMainPresenter Presenter { get; set; }

        public FrmMain()
        {
            InitializeComponent();
        }

        public void ShowNonModal()
        {
            this.Show();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            this.Presenter.ShowFrmAddItemDialog();
        }

        public void InitializeData(IEnumerable<Person> persons)
        {
            lstPerson.Items.Clear();
            lstPerson.Columns.Clear();

            lstPerson.Columns.Add("Name", 60);
            lstPerson.Columns.Add("Surname", -2);

            foreach (var person in persons)
            {
                var li = lstPerson.Items.Add(person.Name);
                li.SubItems.Add(person.Surname);
                li.Tag = person.ID;
            }
        }
    }
}
