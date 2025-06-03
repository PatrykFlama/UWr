using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RepoUoWExample.Model
{
    public interface IParent
    {
        int ID { get; set; }
        string ParentName { get; set; }
        IEnumerable<IChild> Children { get; }
    }

}
