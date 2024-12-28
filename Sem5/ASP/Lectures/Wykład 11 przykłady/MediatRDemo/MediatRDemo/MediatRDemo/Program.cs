using Domain.Logon;
using Microsoft.AspNetCore.Authentication.Cookies;

namespace MediatRDemo
{
    public class Program
    {
        public static void Main( string[] args )
        {
            var builder = WebApplication.CreateBuilder(args);
            builder.Services.AddControllersWithViews();

            builder.Services
                .AddAuthentication( options =>
                {
                    options.DefaultScheme = CookieAuthenticationDefaults.AuthenticationScheme;
                } )
                .AddCookie( CookieAuthenticationDefaults.AuthenticationScheme, options =>
                {
                    options.LoginPath           = "/Account/Logon";
                    options.SlidingExpiration   = true;
                    options.Cookie.SameSite     = SameSiteMode.Unspecified;
                    options.Cookie.SecurePolicy = CookieSecurePolicy.SameAsRequest;
                } );

            builder.Services.AddMediatR( cfg =>
            {
                cfg.RegisterServicesFromAssembly( typeof( LogonUseCaseHandler ).Assembly );
            } );

            var app = builder.Build();

            app.UseRouting();

            app.UseAuthentication();
            app.UseAuthorization();

            app.MapControllerRoute(
                name: "default",
                pattern: "{controller=Home}/{action=Index}/{id?}" );

            app.Run();
        }
    }
}
