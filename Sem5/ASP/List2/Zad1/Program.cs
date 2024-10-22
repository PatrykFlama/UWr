namespace Zad1
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);
            var app = builder.Build();

            app.Map("/{**catchAll}", async (HttpContext context) => {
                var url = $"{context.Request.Scheme}://{context.Request.Host}{context.Request.PathBase}{context.Request.Path}{context.Request.QueryString}";
                var headers = context.Request.Headers.Select(h => $"{h.Key}: {h.Value}");
                var method = context.Request.Method;
                var body = await new StreamReader(context.Request.Body).ReadToEndAsync();
                var response = $"URL: {url}\nMethod: {method}\nHeaders:\n\t{string.Join("\n\t", headers)}\nBody:\n{body}";
                await context.Response.WriteAsync(response);
            });

            app.MapPost("/post", async (HttpContext context) => {
                var method = context.Request.Method;
                var body = await new StreamReader(context.Request.Body).ReadToEndAsync();
                var response = $"Method: {method}\nBody:\n{body}";
                await context.Response.WriteAsync(response);
            });


            app.Run();
        }
    }
}
