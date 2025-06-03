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
    public partial class FrmMain : Form, ISubscriber<PersonAddedNotification>
    {
        public FrmMain()
        {
            InitializeComponent();
            InitializeData();

            EventAggregator.Instance.AddSubscriber<PersonAddedNotification>(this);
        }

        void InitializeData()
        {
            lstPerson.Items.Clear();
            lstPerson.Columns.Clear();

            lstPerson.Columns.Add("Name", 60);
            lstPerson.Columns.Add("Surname", -2);

            using (var uow = new UnitOfWorkFactory().Create())
            {                
                foreach ( var person in uow.Person.GetAll())
                {
                    var li = lstPerson.Items.Add(person.Name);
                    li.SubItems.Add(person.Surname);
                    li.Tag = person.ID;
                }
            }
        }

        private void button1_Click(object sender, EventArgs e)
        {
            new FrmAddItem().ShowDialog();            
        }

        void ISubscriber<PersonAddedNotification>.Handle(PersonAddedNotification Notification)
        {
            InitializeData();
        }

        private void FrmMain_FormClosed(object sender, FormClosedEventArgs e)
        {
            EventAggregator.Instance.RemoveSubscriber<PersonAddedNotification>(this);
        }
    }
}
