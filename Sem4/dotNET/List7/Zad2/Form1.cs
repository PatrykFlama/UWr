namespace Zad2
{
    public partial class Form1 : Form
    {
        private Bar bar;
        private System.Windows.Forms.Timer timer;

        public Form1()
        {
            InitializeComponent();
            
            // create and init
            bar = new Bar();
            bar.Minimum = 0;
            bar.Maximum = 100;
            bar.Dock = DockStyle.Top;

            Controls.Add(bar);

            bar.Value = 100; // set target progress

        }

        //private void Timer_Tick(object sender, EventArgs e)
        //{
        //    // update progressbar value
        //    if (bar.Value < bar.Maximum)
        //    {
        //        bar.Value += 1;
        //    }
        //    else
        //    {
        //        timer.Stop(); // stop timer
        //        bar.Value = bar.Minimum; // reset progress
        //        timer.Start(); // restart timer 
        //    }
        //}


    }
}
