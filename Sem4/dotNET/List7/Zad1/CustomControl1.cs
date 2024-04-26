using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Zad1
{
    public partial class Clock : Control
    {
        public Clock()
        {
            InitializeComponent();
        }

        protected override void OnPaint(PaintEventArgs pe)
        {
            using (Pen p)
            {
                pe.Graphics.DrawEllipse(p, )
            }

            base.OnPaint(pe);
        }
    }
}
