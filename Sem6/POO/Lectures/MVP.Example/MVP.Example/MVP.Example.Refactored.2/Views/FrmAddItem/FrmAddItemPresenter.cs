using MVP.Example.Refactored.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MVP.Example.Refactored
{
    public class FrmAddItemPresenter
    {
        IFrmAddItem view;
        public FrmAddItemPresenter( IFrmAddItem view )
        {
            this.view = view;
            this.view.Presenter = this;
        }

        public void ShowMe()
        {
            this.view.ShowModal();
        }

        public void AddNewPerson( Person person )
        {
            using (var uow = new UnitOfWorkFactory().Create())
            {
                uow.Person.Insert(person);

                EventAggregator.Instance.Publish(new PersonAddedNotification() { NewPerson = person });
            }
        }
    }
}
