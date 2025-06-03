using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MVP.Example.Refactored
{
    public class TestFrmAddItem : IFrmAddItem
    {
        public FrmAddItemPresenter Presenter { get; set; }

        public void ShowModal()
        {
        }
    }
}
