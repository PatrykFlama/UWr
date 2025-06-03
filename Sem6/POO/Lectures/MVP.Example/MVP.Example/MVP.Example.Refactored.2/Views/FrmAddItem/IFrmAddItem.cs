using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MVP.Example.Refactored
{
    public interface IFrmAddItem
    {
        FrmAddItemPresenter Presenter { get; set; }
        void ShowModal();
    }
}
