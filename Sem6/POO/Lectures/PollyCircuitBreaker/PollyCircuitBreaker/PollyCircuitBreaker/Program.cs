using Polly.CircuitBreaker;
using Polly;
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using System.Threading;
using Polly.Wrap;
using System.IO;

namespace PollyCircuitBreaker
{
    internal class Program
    {
        static void Main( string[] args )
        {
            DoWork();

            Console.ReadLine();
        }

        static void DoWork()
        {
            int n = 0;

            while ( true )
            {
                var i = n++;

                Task.Run( async () =>
                {
                    var service         = new ProductService();
                    // identyfikator korelacji
                    string operationKey = Guid.NewGuid().ToString(); 

                    try
                    {
                        // wysłanie do sieci
                        CustomConsole.WriteLine( "DoWork::calls {0} {1}", i, operationKey );
                        var result = await service.GetSample( i, operationKey );

                        CustomConsole.WriteLine( "DoWork::returns {0} = {1} {2}", i, result, operationKey );
                    }
                    catch ( BrokenCircuitException )
                    {
                        // bezpiecznik podniesiony - brak komunikacji
                        CustomConsole.WriteLine( "DoWork::broken circuit handled for {0} {1}", i, operationKey );
                    }
                    catch ( Exception )
                    {
                        // bezpiecznik właśnie podniósł się - błąd komunikacji
                        CustomConsole.WriteLine( "DoWork::failed for {0} {1}", i, operationKey );
                    }
                } );

                Thread.Sleep( 500 );
            }
        }
    }

    /// <summary>
    /// Wywołanie usługi sieciowej
    /// </summary>
    public class ProductService
    {
        private static HttpClient _client = new HttpClient();

        private static readonly AsyncPolicyWrap _policy;

        static ProductService()
        {
            _client.Timeout = TimeSpan.FromSeconds( 3 );

            // polityka ponawiania
            var _retryPolicy = Policy.Handle<Exception>()
                                    .WaitAndRetryAsync( 
                                        4, 
                                        //n => TimeSpan.FromSeconds( Math.Pow(2, n)), 
                                        n => TimeSpan.FromSeconds(1),
                                        (ex, t, ctx) => 
                                        { 
                                            CustomConsole.WriteLine( "retry {0} in {1}", ctx.OperationKey, t ); 
                                        } );

            // polityka "circuit breaker"
            var _cbPolicy = Policy.Handle<Exception>()
                                          .CircuitBreakerAsync( 1, TimeSpan.FromSeconds( 20 ),
                                          ( ex, span, context ) =>
                                          {
                                              CustomConsole.WriteLine( "break {0}", context.OperationKey );

                                              throw new BrokenCircuitException();
                                          },
                                          (context) =>
                                          {
                                              CustomConsole.WriteLine( "on reset {0}", context.OperationKey );
                                          },
                                          () =>
                                          {
                                              CustomConsole.WriteLine( "on half open" );
                                          } );

            // ostateczna polityka to "złożenie" obu
            _policy = _cbPolicy.WrapAsync( _retryPolicy ); 
        }

        public async Task<int> GetSample( int n, string operationKey )
        {
            // policy.ExecuteAsync to proxy (tu "ponawianie + circuit breaker")
            return await _policy.ExecuteAsync<int>( async (c) =>
            {
                // rzeczywiste wywołanie usługi
                var response = await _client.GetAsync("http://localhost:8087/api/user");
                var result   = await response.Content.ReadAsStringAsync();

                return n;
            }, new Context(operationKey) );
        }
    }

    public class CustomConsole
    {
        public static void WriteLine( string message, params object[] ps )
        {
            string s = string.Format( DateTime.Now + " " + message + "\r\n", ps );
            Console.Write( s );
        }
    }
}
