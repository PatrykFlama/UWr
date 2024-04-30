namespace Zad4
{
    partial class Form1
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
            asyncButton = new Button();
            synchButton = new Button();
            textBox = new TextBox();
            SuspendLayout();
            // 
            // asyncButton
            // 
            asyncButton.Location = new Point(162, 215);
            asyncButton.Name = "asyncButton";
            asyncButton.Size = new Size(112, 34);
            asyncButton.TabIndex = 0;
            asyncButton.Text = "asynchroniczne";
            asyncButton.UseVisualStyleBackColor = true;
            asyncButton.Click += asyncButton_Click;
            // 
            // synchButton
            // 
            synchButton.Location = new Point(481, 219);
            synchButton.Name = "synchButton";
            synchButton.Size = new Size(112, 34);
            synchButton.TabIndex = 1;
            synchButton.Text = "synchroniczne";
            synchButton.UseVisualStyleBackColor = true;
            synchButton.Click += synchButton_Click;
            // 
            // textBox
            // 
            textBox.Location = new Point(318, 35);
            textBox.Name = "textBox";
            textBox.Size = new Size(150, 31);
            textBox.TabIndex = 2;
            // 
            // Form1
            // 
            AutoScaleDimensions = new SizeF(10F, 25F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(800, 450);
            Controls.Add(textBox);
            Controls.Add(synchButton);
            Controls.Add(asyncButton);
            Name = "Form1";
            Text = "Form1";
            ResumeLayout(false);
            PerformLayout();
        }

        #endregion

        private Button asyncButton;
        private Button synchButton;
        protected internal TextBox textBox;
    }
}
