namespace Zad1
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);
            var app = builder.Build();


            app.UseStaticFiles();


            app.MapGet("/", async context => {
                context.Response.ContentType = "text/html";
                await context.Response.SendFileAsync("./index.html");
            });

            app.MapGet("/submit-get", async context => {
                var p1 = context.Request.Query["p1"];
                var p2 = context.Request.Query["p2"];
                var message = $"Otrzymano GET z parametrami p1: {p1}, p2: {p2}";

                await context.Response.WriteAsync($@"
                    <html>
                        <body>
                            <p>{message}</p>
                            <a href='/'>Powrót</a>
                        </body>
                    </html>"
                );
            });

            app.MapPost("/submit-post", async context => {
                var form = await context.Request.ReadFormAsync();
                var p1 = form["p1"];
                var p2 = form["p2"];
                var message = $"Otrzymano POST z parametrami p1: {p1}, p2: {p2}";

                await context.Response.WriteAsync($@"
                    <html>
                        <body>
                            <p>{message}</p>
                            <a href='/'>Powrót</a>
                        </body>
                    </html>"
                );
            });



            app.Run();
        }
    }
}
