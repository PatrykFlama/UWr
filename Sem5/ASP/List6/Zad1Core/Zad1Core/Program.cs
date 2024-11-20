using Zad1Core.CusomClasses;

namespace Zad1Core
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);

            // Add services to the container.
            builder.Services.AddControllersWithViews();
            builder.Services.AddSingleton<CMSCustomRouteTransformer>();

            var app = builder.Build();

            app.UseRouting();
            app.UseEndpoints(endpoints =>
            {
                endpoints.MapDynamicControllerRoute<CMSCustomRouteTransformer>("CMS/{**sitepage}");
                endpoints.MapControllerRoute(
                    name: "default",
                    pattern: "{controller=Home}/{action=Index}/{id?}"
                );
            } );


            app.Run();
        }
    }
}
