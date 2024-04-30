using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Zad2
{
    public partial class Bar : UserControl
    {
        private int minimum = 0;
        private int maximum = 100;
        private int value = 0;
        private int targetValue = 0;
        private System.Windows.Forms.Timer timer;
        private const int animationInterval = 20;
        private const int animationStep = 1;

        public Bar()
        {
            DoubleBuffered = true;

            timer = new System.Windows.Forms.Timer();
            timer.Interval = animationInterval;
            timer.Tick += OnTick;
            timer.Start();
        }

        public int Minimum
        {
            get { return minimum; }
            set
            {
                minimum = value;
                Invalidate();
            }
        }

        public int Maximum
        {
            get { return maximum; }
            set
            {
                maximum = value;
                Invalidate();
            }
        }

        public int Value
        {
            get { return value; }
            set
            {
                targetValue = value;
                if (targetValue < minimum)
                    targetValue = minimum;
                else if (targetValue > maximum)
                    targetValue = maximum;
            }
        }

        private void OnTick(object sender, EventArgs e)
        {
            // Update the current value towards the target value
            if (value < targetValue)
            {
                value = Math.Min(value + animationStep, targetValue);
                Invalidate();
            }
            else if (value > targetValue)
            {
                value = Math.Max(value - animationStep, targetValue);
                Invalidate();
            }
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);

            float percent = (float)(value - minimum) / (maximum - minimum);
            int barWidth = (int)(percent * Width);

            // background
            e.Graphics.Clear(BackColor);

            // bar
            using (var brush = new SolidBrush(ForeColor))
            {
                e.Graphics.FillRectangle(brush, 0, 0, barWidth, Height);
            }
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            base.OnSizeChanged(e);
            Invalidate();
        }


    }
}
