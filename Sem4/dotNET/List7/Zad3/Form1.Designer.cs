namespace Zad3
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
            startButton = new Button();
            startThreadButton = new Button();
            SuspendLayout();
            // 
            // startButton
            // 
            startButton.Location = new Point(533, 297);
            startButton.Name = "startButton";
            startButton.Size = new Size(112, 34);
            startButton.TabIndex = 0;
            startButton.Text = "start";
            startButton.UseVisualStyleBackColor = true;
            startButton.Click += startButton_Click;
            // 
            // startThreadButton
            // 
            startThreadButton.Location = new Point(532, 361);
            startThreadButton.Name = "startThreadButton";
            startThreadButton.Size = new Size(112, 34);
            startThreadButton.TabIndex = 1;
            startThreadButton.Text = "start thread";
            startThreadButton.UseVisualStyleBackColor = true;
            startThreadButton.Click += startThreadButton_Click;
            // 
            // Form1
            // 
            AutoScaleDimensions = new SizeF(10F, 25F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(800, 450);
            Controls.Add(startThreadButton);
            Controls.Add(startButton);
            Name = "Form1";
            Text = "Form1";
            ResumeLayout(false);
        }

        #endregion

        private Button startButton;
        private Button startThreadButton;
    }
}
