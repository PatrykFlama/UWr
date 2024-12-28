using ProjectGame.Hubs;

namespace ProjectGame
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);

            // Add services to the container.
            builder.Services.AddControllersWithViews();

            builder.Services.AddSignalR();


            var app = builder.Build();

            app.UseStaticFiles();

            app.MapHub<GameHub>("/gamehub");

            app.UseRouting();

            app.UseAuthorization();

            app.MapControllerRoute(
                name: "default",
                pattern: "{controller=Game}/{action=Index}/{id?}"
            );

            app.Run();
        }
    }
}
