namespace Zad1
{
    partial class Result
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            LabelNazwa = new Label();
            LabelAdres = new Label();
            LabelCyklNauki = new Label();
            LabelTrybStudiow = new Label();
            SuspendLayout();
            // 
            // LabelNazwa
            // 
            LabelNazwa.AutoSize = true;
            LabelNazwa.Location = new Point(12, 9);
            LabelNazwa.Name = "LabelNazwa";
            LabelNazwa.Size = new Size(64, 25);
            LabelNazwa.TabIndex = 0;
            LabelNazwa.Text = "TEMP";
            // 
            // LabelAdres
            // 
            LabelAdres.AutoSize = true;
            LabelAdres.Location = new Point(12, 48);
            LabelAdres.Name = "LabelAdres";
            LabelAdres.Size = new Size(58, 25);
            LabelAdres.TabIndex = 1;
            LabelAdres.Text = "Adres";
            // 
            // LabelCyklNauki
            // 
            LabelCyklNauki.AutoSize = true;
            LabelCyklNauki.Location = new Point(12, 91);
            LabelCyklNauki.Name = "LabelCyklNauki";
            LabelCyklNauki.Size = new Size(95, 25);
            LabelCyklNauki.TabIndex = 2;
            LabelCyklNauki.Text = "Cykl Nauki";
            // 
            // LabelTrybStudiow
            // 
            LabelTrybStudiow.AutoSize = true;
            LabelTrybStudiow.Location = new Point(12, 132);
            LabelTrybStudiow.Name = "LabelTrybStudiow";
            LabelTrybStudiow.Size = new Size(114, 25);
            LabelTrybStudiow.TabIndex = 3;
            LabelTrybStudiow.Text = "Tryb Studiów";
            // 
            // Result
            // 
            AutoScaleDimensions = new SizeF(10F, 25F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(338, 228);
            Controls.Add(LabelTrybStudiow);
            Controls.Add(LabelCyklNauki);
            Controls.Add(LabelAdres);
            Controls.Add(LabelNazwa);
            Name = "Result";
            Text = "Result";
            ResumeLayout(false);
            PerformLayout();
        }

        #endregion

        private Label LabelNazwa;
        private Label LabelAdres;
        private Label LabelCyklNauki;
        private Label LabelTrybStudiow;
    }
}