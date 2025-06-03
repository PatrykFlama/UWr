using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MVP.Example.Refactored.Model.Impl
{
    public class SqlPersonRepository : IPersonRepository
    {
        private SqlConnection conn;
        public SqlPersonRepository( SqlConnection conn )
        {
            this.conn = conn;
        }

        public IEnumerable<Person> GetAll()
        {
            List<Person> ret = new List<Person>();

            using (SqlCommand cmd = new SqlCommand("select * from Person", conn))
            {
                var reader = cmd.ExecuteReader();

                while (reader.Read())
                {
                    Person person = new Person();

                    person.Name = reader["Name"].ToString();
                    person.Surname = reader["Surname"].ToString();
                    person.ID = (int)reader["ID"];

                    ret.Add(person);
                }
            }

            return ret;
        }

        public void Insert(Person person)
        {
            if (person == null || string.IsNullOrEmpty(person.Name) || string.IsNullOrEmpty(person.Surname))
                throw new ArgumentException();

            using (SqlCommand cmd = new SqlCommand("insert into Person (Name, Surname) values (@Name, @Surname)", conn))
            {
                cmd.Parameters.AddWithValue("@Name", person.Name);
                cmd.Parameters.AddWithValue("@Surname", person.Surname);

                cmd.ExecuteNonQuery();
            }            
        }
    }
}
