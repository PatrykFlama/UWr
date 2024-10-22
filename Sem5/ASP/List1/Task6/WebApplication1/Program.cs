namespace WebApplication1
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);
            var app = builder.Build();

            app.MapGet("/", () => "Hello World! (ASP.NET Core)");

            app.MapGet("/getFile", (HttpResponse res) =>
            {
                try
                {
                    var PATH = "C:\\Users\\patry\\Documents\\Programming\\UWr\\Sem5\\ASP\\List1\\text.txt";
                    var PDF_PATH = "C:\\Users\\patry\\Documents\\Programming\\UWr\\Sem5\\ASP\\List1\\z1.pdf";
                    //using(StreamReader sr = File.OpenText(PATH))
                    using(StreamReader sr = new StreamReader(PATH))
                    {
                        return sr.ReadToEnd();
                        //res.SendFileAsync(PDF_PATH);
                    }
                }
                catch(Exception e)
                {
                    return "FileRead Error: " + e.Message;
                }
            });

            app.Run();
        }
    }
}
