using System;
using System.Collections.Generic;
using System.Data.Entity.Core.Common.CommandTrees.ExpressionBuilder;
using System.IO.Ports;
using System.Linq;
using System.Net;
using System.Reflection;
using System.Security.Cryptography;
using System.Text;
using System.Threading.Tasks;

namespace RepoUoWExample.Model
{
    public class UnitOfWorkFactory
    {
        private static Func<IUnitOfWork> _provider;

        public static void SetProvider( Func<IUnitOfWork> provider )
        {
            _provider = provider;
        }

        public IUnitOfWork Create()
        {
            return _provider();
        }
    }
}
