using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using MVP.Example.Refactored.Model;

namespace MVP.Example.Refactored
{
    public class TestFrmMain : IFrmMain
    {
        public FrmMainPresenter Presenter { get; set; }

        private IEnumerable<Person> persons;

        public void InitializeData(IEnumerable<Person> persons)
        {
            this.persons = persons;
        }

        public void ShowNonModal()
        {
        }

        /// <summary>
        /// Właściwość spoza interfejsu, do testów jednostkowych
        /// </summary>
        public int NumberOfPersons
        {
            get
            {
                return persons.Count();
            }
        }
    }
}
