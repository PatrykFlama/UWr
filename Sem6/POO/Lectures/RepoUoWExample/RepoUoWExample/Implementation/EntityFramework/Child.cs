using RepoUoWExample.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RepoUoWExample.Implementation.EntityFramework
{
    public class Child : IChild
    {
        public int ID { get; set; }
        public int ID_PARENT { get; set; }
        public string ChildName { get; set; }
        public virtual Parent Parent { get; set; }
        IParent IChild.Parent 
        {
            get
            {
                return this.Parent;
            }
            set
            {
                if ( value is Parent )
                {
                    this.Parent = (Parent)value;
                }
                else
                {
                    throw new ArgumentException();
                }
            }
        }
    }
}
