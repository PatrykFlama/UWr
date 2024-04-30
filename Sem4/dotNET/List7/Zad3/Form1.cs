using System.ComponentModel;
using System.Windows.Forms;


namespace Zad3
{
    public partial class Form1 : Form
    {
        private BackgroundWorker worker;
        ProgressBar progressBar;
        private Thread calculationThread;
        private bool isCalculating;


        public Form1()
        {
            InitializeComponent();

            progressBar = new ProgressBar();
            progressBar.Minimum = 0;
            progressBar.Maximum = 100;
            progressBar.Value = 0;
            progressBar.Location = new Point(10, 10);
            this.Controls.Add(progressBar);


            worker = new BackgroundWorker();
            worker.WorkerReportsProgress = true;
            worker.DoWork += Worker_Work;
            worker.ProgressChanged += Worker_ProgressChanged;
            worker.RunWorkerCompleted += Worker_RunWorkerCompleted;
        }

        private void startButton_Click(object sender, EventArgs e)
        {
            progressBar.Value = 0;
            if (!worker.IsBusy)
            {
                worker.RunWorkerAsync();
            }
        }

        private void Worker_Work(object sender, DoWorkEventArgs e)
        {
            int totalNumbers = 1000;
            int primeCount = 0;

            for (int i = 2; i <= totalNumbers; i++)
            {
                if (IsPrime(i))
                {
                    primeCount++;
                }

                int progressPercentage = (int)((float)i / totalNumbers * 100);
                worker.ReportProgress(progressPercentage);
                Thread.Sleep(5); // simulate calculation delay
            }

            e.Result = primeCount;
        }

        private void Worker_ProgressChanged(object sender, ProgressChangedEventArgs e)
        {
            progressBar.Value = e.ProgressPercentage;
        }

        private void Worker_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            if (e.Error != null)
            {
                MessageBox.Show("Error occurred: " + e.Error.Message);
            }
            else if (!e.Cancelled)
            {
                int primeCount = (int)e.Result;
                MessageBox.Show($"Found {primeCount} prime numbers.");
            }
        }

        // ----- THREAD ------
        private void startThreadButton_Click(object sender, EventArgs e)
        {
            progressBar.Value = 0;

            if (!isCalculating)
            {
                isCalculating = true;
                calculationThread = new Thread(Thread_Work);
                calculationThread.Start();
            }

        }


        private void Thread_Work()
        {
            int totalNumbers = 1000;
            int primeCount = 0;

            for (int i = 2; i <= totalNumbers; i++)
            {
                if (IsPrime(i))
                {
                    primeCount++;
                }

                int progressPercentage = (int)((float)i / totalNumbers * 100);
                ThreadUpdateProgressBar(progressPercentage);

                Thread.Sleep(5);
            }

            isCalculating = false;
            MessageBox.Show($"Found {primeCount} prime numbers.");
        }

        private void ThreadUpdateProgressBar(int value)
        {
            if (progressBar.InvokeRequired)
            {
                progressBar.BeginInvoke((Action<int>)ThreadUpdateProgressBar, value);
            }
            else
            {
                progressBar.Value = value;
            }
        }


        // ------ MISC ------
        private bool IsPrime(int number)
        {
            if (number <= 1) return false;
            if (number <= 3) return true;
            if (number % 2 == 0 || number % 3 == 0) return false;

            for (int i = 5; i * i <= number; i += 6)
            {
                if (number % i == 0 || number % (i + 2) == 0)
                    return false;
            }

            return true;
        }

    }
}
