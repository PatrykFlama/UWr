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
            Application.Run(new FrmMain());
        }

        static void CompositionRoot()
        {
           UnitOfWorkFactory.SetProvider(() =>
           {
               var cs = ConfigurationManager.ConnectionStrings["cs"].ConnectionString;
               return new SqlUnitOfWork(cs);
           });
        }
    }
}
