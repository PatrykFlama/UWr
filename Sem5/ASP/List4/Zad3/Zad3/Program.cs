using System.ComponentModel.DataAnnotations.Schema;
using System.ComponentModel.DataAnnotations;
using System.Collections.Generic;
using System.Data.SqlClient;
using Dapper;

namespace Zad3
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);
            builder.Services.AddScoped<IDapperRepository, DapperRepository>();

            var app = builder.Build();

            app.MapGet("/", async (IDapperRepository repository) =>
            {
                var data = await repository.GetAllAsync();
                return Results.Ok(data);
            });

            app.Run();
        }
    }
}

