using MVP.Example.Refactored.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MVP.Example.Refactored
{
    public class FrmMainPresenter : IDisposable, ISubscriber<PersonAddedNotification>
    {
        private IFrmMain view;
        public FrmMainPresenter( IFrmMain view )
        {
            this.view = view;
            this.view.Presenter = this;

            this.InitializeData();

            EventAggregator.Instance.AddSubscriber<PersonAddedNotification>(this);
        }

        public void Dispose()
        {
            EventAggregator.Instance.RemoveSubscriber<PersonAddedNotification>(this);
        }

        public void InitializeData()
        {
            using (var uow = new UnitOfWorkFactory().Create())
            {
                this.view.InitializeData(uow.Person.GetAll());
            }
        }

        public void ShowMe()
        {
            this.view.ShowNonModal();
        }

        public FrmAddItemPresenter ShowFrmAddItemDialog()
        {
            var frmAddItem = new ViewFactory().CreateFrmAddItem();
            var frmAddItemPresenter = new FrmAddItemPresenter(frmAddItem);
            frmAddItemPresenter.ShowMe();

            return frmAddItemPresenter;
        }

        void ISubscriber<PersonAddedNotification>.Handle(PersonAddedNotification Notification)
        {
            InitializeData();
        }

    }
}
