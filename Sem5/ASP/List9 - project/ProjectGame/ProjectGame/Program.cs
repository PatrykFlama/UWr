using Microsoft.AspNetCore.Authentication.Cookies;
using ProjectGame.Hubs;
using Data;
using Microsoft.EntityFrameworkCore;
using ProjectGame.Helpers;

namespace ProjectGame
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);

            // DB context
            builder.Services.AddDbContext<AppDbContext>(options =>
                options.UseSqlServer(builder.Configuration.GetConnectionString("DefaultConnection")));

            builder.Services.AddAuthentication(CookieAuthenticationDefaults.AuthenticationScheme)
                .AddCookie(options =>
                {
                    options.LoginPath = "/Account/Login";
                    options.AccessDeniedPath = "/Account/Denied";
                });

            builder.Services.AddScoped<AuthService>();


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
