using Microsoft.Extensions.Options;
using System.Runtime;

namespace Zad4Core
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);
            builder.Configuration.AddJsonFile("myappsettings.json", optional: true, reloadOnChange: true);
            builder.Services.Configure<TypedSettings>(builder.Configuration.GetSection("TypedSettings"));

            var app = builder.Build();

            var appSettingValue = builder.Configuration["AppSettingsKey"];
            var connectionString = builder.Configuration.GetConnectionString("MyDatabase");
            var section = builder.Configuration.GetSection("Section");
            string subVal = section["SubKey"];

            app.MapGet("/", () => $"{appSettingValue} {connectionString} {subVal}");



            app.Run();
        }
    }

    public class TypedSettings
    {
        public string TypedAppSettingKey { get; set; }
    }

    public class TypedSettingsController
    {
        private readonly TypedSettings _settings;

        public TypedSettingsController(IOptions<TypedSettings> options)
        {
            _settings = options.Value;
        }
    }

}
