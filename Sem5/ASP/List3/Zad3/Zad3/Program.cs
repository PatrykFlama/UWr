namespace Zad3
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);
            var app = builder.Build();

            app.MapGet("/", async context => {
                await context.Response.WriteAsync(@$"
                <html>
                    <body>
                        <p>
                            <a href=""/read-headers"">Read headers</a><br>
                            <a href=""/custom-header"">Custom header</a><br>
                            <a href=""/map-path"">Mapping paths</a><br>
                            <a href=""/read-headers"">Read headers</a><br>
                        </p>
                    </body>
                </html>
            ");
            });

            // ------- Headers ---------
            app.MapGet("/read-headers", context =>
            {
                var userAgent = context.Request.Headers["User-Agent"].ToString();

                var headers = string.Join("\n", context.Request.Headers.Select(h => $"{h.Key}: {h.Value}"));

                return context.Response.WriteAsync($"User-Agent: {userAgent}\n\nAll Headers:\n{headers}");
            });

            app.MapGet("/custom-header", context =>
            {
                context.Response.Headers["Hello"] = "world";

                return context.Response.WriteAsync("Added custom header");
            });


            // ---- paths ----
            // https://stackoverflow.com/questions/49398965/what-is-the-equivalent-of-server-mappath-in-asp-net-core
            app.MapGet("/map-path", (IWebHostEnvironment env) =>
            {
                var virtualPath = "Program.cs";
                var physicalPath = Path.Combine(env.ContentRootPath, virtualPath);

                return Results.Text($"Physical path: {physicalPath}");
            });


            // ------- HttpContext.Current ----
            /*
            statyczna w³asnoœæ w .net framework pozwalaj¹ca na dostêp do kontekstu z dowolnego 
            miejsca w kodzie
            w asp net core korzystamy z HttpContextAccessor
            */





            app.Run();
        }
    }
}
