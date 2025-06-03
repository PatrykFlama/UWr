// flaga kompilacji lub if - uruchomienie normalne vs test jednostkowy
// zakomentować żeby dostać uruchomienie testu

//#define REGULARRUN

using MVP.Example.Refactored.Model;
using MVP.Example.Refactored.Model.Impl;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace MVP.Example.Refactored
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            CompositionRoot();

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);

#if (REGULARRUN)

            // uruchomienie aplikacji w normalnym trybie
            var frmMain = new ViewFactory().CreateFrmMain();
            using (var frmMainPresenter = new FrmMainPresenter( frmMain ))
            {
                frmMainPresenter.ShowMe();

                Application.Run( frmMain as Form );                
            }

#else

            TestFrmMain frmMain = (TestFrmMain)new ViewFactory().CreateFrmMain();
            using (var frmMainPresenter = new FrmMainPresenter(frmMain))
            {
                frmMainPresenter.ShowMe();

                var numberOfPersons = frmMain.NumberOfPersons;

                // "pokazuje" dialog dodawania nowego elementu
                var frmAddItemPresenter = frmMainPresenter.ShowFrmAddItemDialog(); 
                // dodaje nowy element
                frmAddItemPresenter.AddNewPerson(new Person() { Name = DateTime.Now.ToString(), Surname = DateTime.Now.ToString() });
                
                var newNumberOfPersons = frmMain.NumberOfPersons;

                // powinno być Assert.AreEqual( newNumberOfPersons, numberOfPersons + 1 )
                Console.WriteLine("{0} {1}", numberOfPersons, newNumberOfPersons);
            }

            Console.ReadLine();

#endif
        }

        static void CompositionRoot()
        {
#if (REGULARRUN)

            ViewFactory.SetProvider(new WindowsFormsViewFactory());

#else

            ViewFactory.SetProvider(new TestViewFactory());

#endif

            UnitOfWorkFactory.SetProvider(() =>
            {
               var cs = ConfigurationManager.ConnectionStrings["cs"].ConnectionString;
               return new SqlUnitOfWork(cs);
            });
        }
    }

#if (REGULARRUN)

    /// <summary>
    /// Fabryka rzeczywistych widoków
    /// </summary>
    public class WindowsFormsViewFactory : IViewFactory
    {
        public IFrmAddItem CreateFrmAddItem()
        {
            return new FrmAddItem();
        }

        public IFrmMain CreateFrmMain()
        {
            return new FrmMain();
        }
    }

#else

    /// <summary>
    /// Fabryka widoków testowych
    /// </summary>
    public class TestViewFactory : IViewFactory
    {
        public IFrmAddItem CreateFrmAddItem()
        {
            return new TestFrmAddItem();
        }

        public IFrmMain CreateFrmMain()
        {
            return new TestFrmMain();
        }
    }


#endif
}
