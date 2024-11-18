using System.Collections.Generic;
using Zad6.Models;
using System.Threading.Tasks;
using System;
using Dapper;
using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.Extensions.Configuration;
using Microsoft.Data.SqlClient;
using System.Data;


namespace Zad6.Data
{
    public interface IDatabaseConnection
    {
        IEnumerable<PersonModel> GetAllPersons();
        PersonModel GetPersonById(int id);
        int AddPerson(PersonModel person);
        int UpdatePerson(PersonModel person);
        int DeletePerson(int id);
    }


    public class DapperRepository : IDatabaseConnection
    {
        private readonly IConfiguration _configuration;

        public DapperRepository(IConfiguration configuration)
        {
            _configuration = configuration;
        }


        private SqlConnection Connection => new SqlConnection(_configuration.GetConnectionString("DefaultConnection"));

        public IEnumerable<PersonModel> GetAllPersons()
        {
            using(var connection = Connection)
            {
                connection.Open();
                var query = "SELECT * FROM Person"; // Adjust query to your table name
                return connection.Query<PersonModel>(query);
            }
        }

        public PersonModel GetPersonById(int id)
        {
            using(var connection = Connection)
            {
                connection.Open();
                var query = "SELECT * FROM Person WHERE ID = @Id"; // Adjust query to your table name
                return connection.QueryFirstOrDefault<PersonModel>(query, new { Id = id });
            }
        }

        //public async Task<PersonModel> GetPersonByIdAsync(int id)
        //{
        //    using(var connection = Connection)
        //    {
        //        connection.Open();
        //        var query = "SELECT * FROM Person WHERE ID = @Id"; // Adjust query to your table name
        //        return await connection.QueryFirstOrDefaultAsync<PersonModel>(query, new { Id = id });
        //    }
        //}

        public int AddPerson(PersonModel person)
        {
            using(var connection = Connection)
            {
                connection.Open();
                var query = "INSERT INTO Person (Name, Surname) VALUES (@Name, @Surname)";
                return connection.Execute(query, person);
            }
        }

        public int UpdatePerson(PersonModel person)
        {
            using(var connection = Connection)
            {
                connection.Open();
                var query = "UPDATE Person SET Name = @Name, Surname = @Surname WHERE ID = @ID";
                return connection.Execute(query, person);
            }
        }

        public int DeletePerson(int id)
        {
            using(var connection = Connection)
            {
                connection.Open();
                var query = "DELETE FROM Person WHERE ID = @Id";
                return connection.Execute(query, new  { Id = id });
            }
        }
    }
}
