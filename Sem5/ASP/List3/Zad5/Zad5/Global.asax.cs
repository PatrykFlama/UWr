using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Web;
using System.Web.Security;
using System.Web.SessionState;

namespace Zad5
{
    public class Global : System.Web.HttpApplication
    {
        protected void Application_Start(object sender, EventArgs e)
        {
        }

        // zwalniamy połączenie z bazą wraz z zakończeniem zapytania
        protected void Application_EndRequest(object sender, EventArgs e)
        {
            if(HttpContext.Current.Items["DataContextKey"] is SqlConnection connection)
            {
                connection.Dispose();
            }
        }
    }

    public static class DBConnect
    {
        private const string CONTEXT_KEY = "DataContextKey";

        public static SqlConnection Current
        {
            get
            {
                if(HttpContext.Current.Items[CONTEXT_KEY] == null)
                {
                    var connectionString = "YourConnectionStringHere";
                    var sqlConnection = new SqlConnection(connectionString);
                    sqlConnection.Open();

                    HttpContext.Current.Items[CONTEXT_KEY] = sqlConnection;
                }

                return (SqlConnection)HttpContext.Current.Items[CONTEXT_KEY];
            }
        }
    }
}