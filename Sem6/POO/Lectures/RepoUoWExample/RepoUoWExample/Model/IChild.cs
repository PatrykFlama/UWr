using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RepoUoWExample.Model
{
    public interface IChild
    {
        int ID { get; set; }
        int ID_PARENT { get; set; }
        string ChildName { get; set; }
        IParent Parent { get; set; }
    }

}
