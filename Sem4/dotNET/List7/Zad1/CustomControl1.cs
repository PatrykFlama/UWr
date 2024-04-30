using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Windows.Forms;

namespace Zad1
{
    public partial class Clock : Control
    {
        private System.Windows.Forms.Timer timer;

        public Clock()
        {
            InitializeClock();

            timer = new System.Windows.Forms.Timer();
            timer.Interval = 1000;
            timer.Tick += Timer_Tick;
            timer.Start();
        }

        private void InitializeClock()
        {
            this.Width = 400;
            this.Height = 400;

            this.DoubleBuffered = true;
        }

        private void Timer_Tick(object sender, EventArgs e)
        {
            // call invalidate to draw again
            this.Invalidate();
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);

            // calc data
            int width = this.Width;
            int height = this.Height;
            Point center = new Point(width / 2, height / 2);
            int radius = Math.Min(width, height) / 2 - 10;

            // draw clock template
            Graphics g = e.Graphics;    // get graphical context
            g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias;
            g.Clear(Color.White);
            g.DrawEllipse(Pens.Black, center.X - radius, center.Y - radius, 2 * radius, 2 * radius);

            // draw clock time
            DateTime currentTime = DateTime.Now;
            DrawClockTime(g, center, radius, currentTime);
        }

        private void DrawClockTime(Graphics g, Point center, int radius, DateTime currentTime)
        {
            double hourAngle = (currentTime.Hour % 12 + currentTime.Minute / 60.0) * 30; // 30 deg/h
            double minuteAngle = currentTime.Minute * 6; // 6 deg/min
            double secondAngle = currentTime.Second * 6; // 6 deg/s

            // hour
            DrawHand(g, center, radius * 0.5, hourAngle, Pens.Black, 4);

            // minutes
            DrawHand(g, center, radius * 0.8, minuteAngle, Pens.Blue, 2);

            // seconds
            DrawHand(g, center, radius * 0.8, secondAngle, Pens.Red, 1);
        }

        private void DrawHand(Graphics g, Point center, double length, double angleDegrees, Pen pen, int thickness)
        {
            double angleRadians = angleDegrees * Math.PI / 180;
            Point endPoint = new Point(center.X + (int)(length * Math.Sin(angleRadians)),
                                       center.Y - (int)(length * Math.Cos(angleRadians)));
            g.DrawLine(new Pen(pen.Color, thickness), center, endPoint);
        }


        // to update clock size, when control is resized
        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);

            int newSize = Math.Min(this.Width, this.Height);
            this.Width = newSize;
            this.Height = newSize;
        }


    }
}
