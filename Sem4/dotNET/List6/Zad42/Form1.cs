using System.Configuration;

namespace Zad42
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            string s = ConfigurationManager.AppSettings["StringParam"];
            int i = Int32.Parse(ConfigurationManager.AppSettings["IntParam"]);
            bool b = Boolean.Parse(ConfigurationManager.AppSettings["BoolParam"]);

            MessageBox.Show($"{s} {i} {b}");
        }
    }

    public class SettingsClass : ApplicationSettingsBase
    {
        void gimmeSettings()
        {

            // https://learn.microsoft.com/en-us/dotnet/desktop/winforms/advanced/application-settings-architecture?view=netframeworkdesktop-4.8&redirectedfrom=MSDN
            /* in config
            <setting name="InitText" serializeAs="String">
			    <value>Hello</value>
		    </setting>
		    <setting name="StartAt" serializeAs="Integer">
			    <value>5</value>
		    </setting>
		    <setting name="IsWeekend" serializeAs="Boolean">
			    <value>True</value>
		    </setting>
             */

            //int i = Settings.Default.IntSetting;
            //bool b = Settings.Default.BoolSetting;
        }
    }
}
