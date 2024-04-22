using System.Configuration;

namespace Zad4
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            // Odczytanie parametrów z pliku konfiguracyjnego
            string stringParam = ConfigurationManager.AppSettings["StringParam"];
            int intParam = Convert.ToInt32(ConfigurationManager.AppSettings["IntParam"]);
            bool boolParam = Convert.ToBoolean(ConfigurationManager.AppSettings["BoolParam"]);

            // Wyœwietlenie odczytanych wartoœci
            MessageBox.Show($"StringParam: {stringParam}\nIntParam: {intParam} BoolParam: {boolParam}");
        }
    }
}
