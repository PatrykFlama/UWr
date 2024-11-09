using System.ComponentModel.DataAnnotations.Schema;
using System.ComponentModel.DataAnnotations;
using System.Collections.Generic;

namespace Zad3
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);
            builder.Services.AddScoped<IDapperRepository<Person>, DapperRepository<Person>>();

            var app = builder.Build();

            app.MapGet("/", async (IDapperRepository<Person> repository) =>
            {
                var data = await repository.Get();
                return Results.Ok(data);
            });

            app.Run();
        }
    }


    [Table("Person")]
    public class Person
    {
        [Key]
        public int ID { get; set; }
        public string Name { get; set; }
        public string Surname { get; set; }
    }

    public interface IDapperRepository<T>
    {
        IEnumerable<T> Get(string query, object parameters);
        int? Insert(T t);
        int Update(T t);
        int Delete(T t);
    }


    public class DapperRepository<T> : IDapperRepository<T>, IDisposable
    {
        IConfiguration _configuration;
        SqlConnection _connection;
        public DapperRepository(IConfiguration configuration)
        {
            _configuration = configuration;
            var connectionString = configuration["AppSettings:ConnectionString"];
            _connection = new SqlConnection(connectionString);
        }

        public int Delete(T t)
        {
            return this._connection.Delete(t);
        }

        public void Dispose()
        {
            _connection.Dispose();
        }
        public IEnumerable<T> Get(string query, object parameters)
        {
            return this._connection.Query<T>(query, parameters);
        }
        public int? Insert(T t)
        {
            return this._connection.Insert(t);
        }
        public int Update(T t)
        {
            return this._connection.Update(t);
        }
    }
}

