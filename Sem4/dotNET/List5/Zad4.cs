using System.Net.Http;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

namespace Zad4
{
    public static class StringExtensions
    {
        public static HttpClient client = new HttpClient();
        public static async Task<string> DownloadContentAsync(string url)
        {
            HttpResponseMessage response = await client.GetAsync(url);
            response.EnsureSuccessStatusCode();

            return await response.Content.ReadAsStringAsync();
        }

        public static TaskAwaiter<string> GetAwaiter(this string url)
        {
            return DownloadContentAsync(url).GetAwaiter();
        }
    }

    class Program4
    {
        public static async Task Main(string[] args)
        {
            string url = "https://www.google.com";
            string content = await url;

            Console.WriteLine(content);
        }
    }
}
