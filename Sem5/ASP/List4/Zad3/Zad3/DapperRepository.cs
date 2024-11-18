using Dapper;
using System.Data;
using System.Data.SqlClient;


namespace Zad3
{
    public interface IDapperRepository : IDisposable
    {
        public Task<IEnumerable<object>> GetAllAsync();
    }

    public class DapperRepository : IDapperRepository
    {
        private readonly IDbConnection _connection;

        public DapperRepository(IConfiguration configuration)
        {
            _connection = new SqlConnection(configuration.GetConnectionString("DefaultConnection"));
        }
        public async Task<IEnumerable<object>> GetAllAsync()
        {
            return await _connection.QueryAsync<object>("SELECT * FROM Table");
        }
        public void Dispose()
        {
            _connection?.Dispose();
        }

    }

    public static class DapperRepositoryExtensions
    {
        public static IApplicationBuilder UseDapperRepository(this IApplicationBuilder builder)
        {
            return builder.UseMiddleware<DapperRepository>();
        }
    }
}
