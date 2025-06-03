using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MVP.Example.Refactored.Model.Impl
{
    public class SqlUnitOfWork : IUnitOfWork
    {
        private SqlConnection conn;

        public SqlUnitOfWork( string connectionString )
        {
            this.conn = new SqlConnection(connectionString);
            this.conn.Open();
        }

        private IPersonRepository _person;
        public IPersonRepository Person
        {
            get
            {
                if (_person == null)
                    _person = new SqlPersonRepository(this.conn);
                return _person;
            }
        }

        public void Dispose()
        {
            if (this.conn != null)
                this.conn.Dispose();
        }
    }
}
