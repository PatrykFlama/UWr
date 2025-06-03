using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MVP.Example.Refactored
{
    public class ViewFactory
    {
        private static IViewFactory _provider;

        public static void SetProvider( IViewFactory provider )
        {
            _provider = provider;
        }

        public IFrmAddItem CreateFrmAddItem()
        {
            return _provider.CreateFrmAddItem();
        }

        public IFrmMain CreateFrmMain()
        {
            return _provider.CreateFrmMain();
        }
    }

    public interface IViewFactory
    {
        IFrmAddItem CreateFrmAddItem();
        IFrmMain CreateFrmMain();
    }
}
