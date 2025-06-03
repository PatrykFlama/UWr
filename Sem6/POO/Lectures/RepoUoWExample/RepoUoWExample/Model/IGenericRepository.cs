using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RepoUoWExample.Model
{
    public interface IGenericRepository<T>
    {
        T New();

        void Insert( T t );
        void Update( T t );
        void Delete( T t );

        IQueryable<T> Query { get; }
    }

}
