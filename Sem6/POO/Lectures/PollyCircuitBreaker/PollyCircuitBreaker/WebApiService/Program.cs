using System;
using System.Threading.Tasks;
using System.Web.Http;
using System.Web.Http.SelfHost;

namespace WebApiService
{
    internal class Program
    {
        static void Main( string[] args )
        {
            var config = new HttpSelfHostConfiguration("http://localhost:8087");

            config.Routes.MapHttpRoute(
                "API Default", "api/{controller}/{id}",
                new { id = RouteParameter.Optional } );

            using ( HttpSelfHostServer server = new HttpSelfHostServer( config ) )
            {
                server.OpenAsync().Wait();
                Console.WriteLine( "Press Enter to quit." );
                Console.ReadLine();
            }
        }
    }

    public class UserController : ApiController
    {
        public async Task<IHttpActionResult> Get()
        {
            await Task.Delay( 2000 );

            return this.Ok( 1 );
        }
    }
}
