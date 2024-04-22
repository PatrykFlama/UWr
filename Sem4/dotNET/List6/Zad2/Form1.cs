namespace Zad2
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void option2ToolStripMenuItem_Click(object sender, EventArgs e)
        {

        }

        private void toolStripLabel1_Click(object sender, EventArgs e)
        {

        }

        private void toolStripButton1_Click(object sender, EventArgs e)
        {
            contextMenuStrip1.Show(0, 0);
        }

        private void button1_Click(object sender, EventArgs e)
        {
            Button newButton = new Button();
            newButton.Name = "Name";
            newButton.Text = "Text";
            panel1.Controls.Add(newButton);
        }

        private void button2_Click(object sender, EventArgs e)
        {
            Button newButton = new Button();
            newButton.Name = "Name";
            newButton.Text = "Text";
            flowLayoutPanel1.Controls.Add(newButton);
        }
    }
}
