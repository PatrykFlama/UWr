namespace Zad1
{
    public partial class WyborUczelni : Form
    {
        public WyborUczelni()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {

        }

        private void groupBox1_Enter(object sender, EventArgs e)
        {

        }

        private void textBox1_TextChanged(object sender, EventArgs e)
        {

        }

        private void label1_Click(object sender, EventArgs e)
        {

        }

        private void label2_Click(object sender, EventArgs e)
        {

        }

        private void comboBox1_SelectedIndexChanged(object sender, EventArgs e)
        {

        }

        private void checkBox2_CheckedChanged(object sender, EventArgs e)
        {
            DzienneCheck.Checked = !UzupelniajaceCheck.Checked;
        }

        private void AnulujButton_Click(object sender, EventArgs e)
        {
            Application.Exit();
        }

        private void AkceptujButton_Click(object sender, EventArgs e)
        {
            //Result resForm = new Result();
            //resForm.FormClosed += (s, args) => this.Show();
            //this.Hide();
            //resForm.Show();

            MessageBox.Show(NazwaText.Text + "\n" + AdresText.Text + "\n" + CyklNaukiCombo.Text + "\n" + (DzienneCheck.Checked == true ? "\nDzienne" : (UzupelniajaceCheck.Checked == true? "\nUzupe³niaj¹ce" : "")), "Uczelnia");

            this.Close();
        }

        private void DzienneCheck_CheckedChanged(object sender, EventArgs e)
        {
            UzupelniajaceCheck.Checked = !DzienneCheck.Checked;
        }
    }
}
