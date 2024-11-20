using Microsoft.AspNetCore.Mvc;
using Microsoft.Data.SqlClient;

namespace Zad2.Controllers
{
	public class DataController : Controller
	{
		private readonly SqlConnection _connection;

		public DataController(SqlConnection connection)
		{
			_connection = connection;
		}

		public IActionResult Index()
		{
			return Content("Połączenie do bazy danych zostało wstrzyknięte.");
		}

	}
}
