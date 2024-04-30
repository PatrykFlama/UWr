using System.Net;

namespace Zad4
{
    public partial class Form1 : Form
    {
        private System.Windows.Forms.Timer updateTimer;
        private ProgressBar progressBar;

        public Form1()
        {
            InitializeComponent();

            updateTimer = new System.Windows.Forms.Timer();
            updateTimer.Interval = 50;
            updateTimer.Tick += UpdateContent; 
            updateTimer.Start();


            progressBar = new ProgressBar();
            progressBar.Minimum = 0;
            progressBar.Maximum = 100;
            progressBar.Dock = DockStyle.Bottom;
            Controls.Add(progressBar);

        }

        private async void asyncButton_Click(object sender, EventArgs e)
        {
            string url = "https://google.com";
            HttpClient httpClient = new HttpClient();
            try
            {
                string content = await httpClient.GetStringAsync(url);
                textBox.Text = content;
            }
            catch (Exception ex)
            {
                MessageBox.Show("B³¹d pobierania zawartoœci: " + ex.Message);
            }
            finally
            {
                httpClient.Dispose();
            }
        }

        private void synchButton_Click(object sender, EventArgs e)
        {
            string url = "https://google.com";
            WebClient webClient = new WebClient();
            try
            {
                string content = webClient.DownloadString(url);
                textBox.Text = content;
            }
            catch (Exception ex)
            {
                MessageBox.Show("B³¹d pobierania zawartoœci: " + ex.Message);
            }
            finally
            {
                webClient.Dispose();
            }
        }


        private async void UpdateContent(object sender, EventArgs e)
        {

            if (progressBar.Value < progressBar.Maximum)
            {
                progressBar.Value += 1; 
            }
            else
            {
                progressBar.Value = 0;
            }
        }


    }
}
