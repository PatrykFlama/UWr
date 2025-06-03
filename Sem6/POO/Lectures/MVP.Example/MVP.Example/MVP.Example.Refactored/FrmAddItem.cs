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

            if ( !string.IsNullOrEmpty(name) &&
                 !string.IsNullOrEmpty(surname))
            {
                using ( var uow = new UnitOfWorkFactory().Create())
                {
                    try
                    {
                        Person person = new Person();
                        person.Name = name;
                        person.Surname = surname;

                        uow.Person.Insert(person);
                        
                        this.Close();

                        EventAggregator.Instance.Publish(new PersonAddedNotification() { NewPerson = person });
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show(ex.Message);
                    }
                }
            }
        }

        private void button2_Click(object sender, EventArgs e)
        {
            this.Close();
        }
    }
}
