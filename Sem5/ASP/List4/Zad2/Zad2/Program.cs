using Microsoft.AspNetCore.Routing.Matching;

namespace Zad2
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);
            builder.Services.AddSingleton<EndpointSelector, CustomEndpointSelector>();

            var app = builder.Build();
            app.UseRouting();


            app.MapGet("/int/{parameter:int:minlength(3):maxlength(10):required}", (int parameter) =>
            {
                return Results.Ok($"Integer: {parameter}");
            });

            app.MapGet("/string/{parameter:alpha:minlength(3):maxlength(10):regex(^[A-Ca-c]+$)}", (string parameter) =>
            {
                return Results.Ok($"String: {parameter}");
            });

            app.MapGet("/err/{parameter:int}", (int parameter) =>
            {
                return Results.Ok($"Err int: {parameter}");
            });

            app.MapGet("/err/{parameter:int:minlength(2)}", (string parameter) =>
            {
                return Results.Ok($"Err int2: {parameter}");
            });


            app.UseEndpoints((endpoints) => { });
            app.Run();
        }
    }

    class CustomEndpointSelector : EndpointSelector
    {
        public override async Task SelectAsync(HttpContext httpContext, CandidateSet candidates)
        {
            CandidateState selectedCandidate = new CandidateState();

            for(var i = 0; i < candidates.Count; i++)
            {
                if(candidates.IsValidCandidate(i))
                {
                    selectedCandidate = candidates[i];
                    break;
                }
            }

            httpContext.SetEndpoint(selectedCandidate.Endpoint);
            httpContext.Request.RouteValues = selectedCandidate.Values;
        }
    }

}
