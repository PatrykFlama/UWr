using MVP.Example.Refactored.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MVP.Example.Refactored
{
    public interface IFrmMain
    {
        FrmMainPresenter Presenter { get; set; }
        void InitializeData(IEnumerable<Person> persons);
        void ShowNonModal();
    }
}
