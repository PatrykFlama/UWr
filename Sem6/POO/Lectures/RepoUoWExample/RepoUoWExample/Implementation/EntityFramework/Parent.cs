using RepoUoWExample.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RepoUoWExample.Implementation.EntityFramework
{
    public class Parent : IParent
    {
        public int ID { get; set; }
        public string ParentName { get; set; }
        public virtual ICollection<Child> Children { get; set; }
        IEnumerable<IChild> IParent.Children { get => this.Children; }
    }
}
