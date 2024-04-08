namespace Zad1
{
    partial class WyborUczelni
    {
        /// <summary>
        ///  Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        ///  Clean up any resources being used.
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
        ///  Required method for Designer support - do not modify
        ///  the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            UczelniaBox = new GroupBox();
            AdresLabel = new Label();
            NazwaLabel = new Label();
            AdresText = new TextBox();
            NazwaText = new TextBox();
            RodzajStudiowBox = new GroupBox();
            label3 = new Label();
            UzupelniajaceCheck = new CheckBox();
            DzienneCheck = new CheckBox();
            CyklNaukiCombo = new ComboBox();
            AkceptujButton = new Button();
            AnulujButton = new Button();
            UczelniaBox.SuspendLayout();
            RodzajStudiowBox.SuspendLayout();
            SuspendLayout();
            // 
            // UczelniaBox
            // 
            UczelniaBox.Controls.Add(AdresLabel);
            UczelniaBox.Controls.Add(NazwaLabel);
            UczelniaBox.Controls.Add(AdresText);
            UczelniaBox.Controls.Add(NazwaText);
            UczelniaBox.Location = new Point(12, 12);
            UczelniaBox.Name = "UczelniaBox";
            UczelniaBox.Size = new Size(776, 150);
            UczelniaBox.TabIndex = 0;
            UczelniaBox.TabStop = false;
            UczelniaBox.Text = "Uczelnia";
            UczelniaBox.Enter += groupBox1_Enter;
            // 
            // AdresLabel
            // 
            AdresLabel.AutoSize = true;
            AdresLabel.Location = new Point(6, 86);
            AdresLabel.Name = "AdresLabel";
            AdresLabel.Size = new Size(62, 25);
            AdresLabel.TabIndex = 3;
            AdresLabel.Text = "Adres:";
            AdresLabel.Click += label2_Click;
            // 
            // NazwaLabel
            // 
            NazwaLabel.AutoSize = true;
            NazwaLabel.Location = new Point(6, 46);
            NazwaLabel.Name = "NazwaLabel";
            NazwaLabel.Size = new Size(68, 25);
            NazwaLabel.TabIndex = 2;
            NazwaLabel.Text = "Nazwa:";
            NazwaLabel.Click += label1_Click;
            // 
            // AdresText
            // 
            AdresText.Location = new Point(119, 86);
            AdresText.Name = "AdresText";
            AdresText.Size = new Size(651, 31);
            AdresText.TabIndex = 1;
            // 
            // NazwaText
            // 
            NazwaText.Location = new Point(119, 46);
            NazwaText.Name = "NazwaText";
            NazwaText.Size = new Size(651, 31);
            NazwaText.TabIndex = 0;
            NazwaText.TextChanged += textBox1_TextChanged;
            // 
            // RodzajStudiowBox
            // 
            RodzajStudiowBox.Controls.Add(label3);
            RodzajStudiowBox.Controls.Add(UzupelniajaceCheck);
            RodzajStudiowBox.Controls.Add(DzienneCheck);
            RodzajStudiowBox.Controls.Add(CyklNaukiCombo);
            RodzajStudiowBox.Location = new Point(12, 168);
            RodzajStudiowBox.Name = "RodzajStudiowBox";
            RodzajStudiowBox.Size = new Size(776, 150);
            RodzajStudiowBox.TabIndex = 1;
            RodzajStudiowBox.TabStop = false;
            RodzajStudiowBox.Text = "Rodzaj Studiów";
            // 
            // label3
            // 
            label3.AutoSize = true;
            label3.Location = new Point(6, 49);
            label3.Name = "label3";
            label3.Size = new Size(96, 25);
            label3.TabIndex = 3;
            label3.Text = "Cykl nauki:";
            // 
            // UzupelniajaceCheck
            // 
            UzupelniajaceCheck.AutoSize = true;
            UzupelniajaceCheck.Location = new Point(381, 96);
            UzupelniajaceCheck.Name = "UzupelniajaceCheck";
            UzupelniajaceCheck.Size = new Size(146, 29);
            UzupelniajaceCheck.TabIndex = 2;
            UzupelniajaceCheck.Text = "Uzupełniające";
            UzupelniajaceCheck.UseVisualStyleBackColor = true;
            UzupelniajaceCheck.CheckedChanged += checkBox2_CheckedChanged;
            // 
            // DzienneCheck
            // 
            DzienneCheck.AutoSize = true;
            DzienneCheck.Location = new Point(119, 96);
            DzienneCheck.Name = "DzienneCheck";
            DzienneCheck.Size = new Size(101, 29);
            DzienneCheck.TabIndex = 1;
            DzienneCheck.Text = "Dzienne";
            DzienneCheck.UseVisualStyleBackColor = true;
            // 
            // CyklNaukiCombo
            // 
            CyklNaukiCombo.FormattingEnabled = true;
            CyklNaukiCombo.Items.AddRange(new object[] { "3-letnie", "3.5-letnie", "5-letnie" });
            CyklNaukiCombo.Location = new Point(119, 49);
            CyklNaukiCombo.Name = "CyklNaukiCombo";
            CyklNaukiCombo.Size = new Size(651, 33);
            CyklNaukiCombo.TabIndex = 0;
            CyklNaukiCombo.SelectedIndexChanged += comboBox1_SelectedIndexChanged;
            // 
            // AkceptujButton
            // 
            AkceptujButton.Location = new Point(12, 404);
            AkceptujButton.Name = "AkceptujButton";
            AkceptujButton.Size = new Size(112, 34);
            AkceptujButton.TabIndex = 2;
            AkceptujButton.Text = "Akceptuj";
            AkceptujButton.UseVisualStyleBackColor = true;
            AkceptujButton.Click += AkceptujButton_Click;
            // 
            // AnulujButton
            // 
            AnulujButton.Anchor = AnchorStyles.Top | AnchorStyles.Right;
            AnulujButton.Location = new Point(676, 404);
            AnulujButton.Name = "AnulujButton";
            AnulujButton.Size = new Size(112, 34);
            AnulujButton.TabIndex = 4;
            AnulujButton.Text = "Anuluj";
            AnulujButton.UseVisualStyleBackColor = true;
            AnulujButton.Click += AnulujButton_Click;
            // 
            // WyborUczelni
            // 
            AutoScaleDimensions = new SizeF(10F, 25F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(800, 450);
            Controls.Add(AnulujButton);
            Controls.Add(AkceptujButton);
            Controls.Add(RodzajStudiowBox);
            Controls.Add(UczelniaBox);
            MaximizeBox = false;
            Name = "WyborUczelni";
            Text = "Wybór uczelni";
            Load += Form1_Load;
            UczelniaBox.ResumeLayout(false);
            UczelniaBox.PerformLayout();
            RodzajStudiowBox.ResumeLayout(false);
            RodzajStudiowBox.PerformLayout();
            ResumeLayout(false);
        }

        #endregion

        private GroupBox UczelniaBox;
        private GroupBox RodzajStudiowBox;
        private TextBox AdresText;
        private TextBox NazwaText;
        private CheckBox UzupelniajaceCheck;
        private CheckBox DzienneCheck;
        private ComboBox CyklNaukiCombo;
        private Label AdresLabel;
        private Label NazwaLabel;
        private Label label3;
        private Button AkceptujButton;
        private Button AnulujButton;
    }
}
