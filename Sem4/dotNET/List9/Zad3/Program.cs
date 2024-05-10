using System;
using System.Net;
using System.Net.Http;
using System.Net.Mail;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        // HttpClient do pobrania zawartości strony
        var p1 = UseHttpClient();

        // nasłuchiwanie połączeń TCP za pomocą TcpListener
        var p2 = UseTcpListener();

        // wysyłanie e-maila za pomocą SmtpClient
        var p3 = UseSmtpClient();

        await p1;
        await p2;
        await p3;
    }

    static async Task UseHttpClient()
    {
        Console.WriteLine("------ HttpClient ------");
        using (var client = new HttpClient())
        {
            try
            {
                HttpResponseMessage response = await client.GetAsync("https://motherfuckingwebsite.com/");
                response.EnsureSuccessStatusCode();
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine("odpowiedź z serwera:");
                Console.WriteLine(responseBody);
            }
            catch (HttpRequestException ex)
            {
                Console.WriteLine($"błąd HTTP: {ex.Message}");
            }
        }
        Console.WriteLine();
    }

    static async Task UseTcpListener()
    {
        Console.WriteLine("------ TcpListener ------");
        var listener = new TcpListener(IPAddress.Any, 1234);
        listener.Start();

        try
        {
            Console.WriteLine("nasłuchiwanie na porcie 1234");

            while (true)
            {
                TcpClient client = await listener.AcceptTcpClientAsync();
                Console.WriteLine("połączenie nawiązane");

                NetworkStream stream = client.GetStream();
                byte[] buffer = new byte[1024];
                int bytesRead = await stream.ReadAsync(buffer, 0, buffer.Length);
                string dataReceived = Encoding.UTF8.GetString(buffer, 0, bytesRead);
                Console.WriteLine($"odebrano: {dataReceived}");

                byte[] responseBuffer = Encoding.UTF8.GetBytes("wiadomość odebrana przez serwer TCP");
                await stream.WriteAsync(responseBuffer, 0, responseBuffer.Length);

                client.Close();
            }
        }
        finally
        {
            listener.Stop();
        }
    }

    static async Task UseSmtpClient()
    {
        Console.WriteLine("------ SmtpClient ------");
        var client = new SmtpClient("smtp.gmail.com", 587)
        {
            UseDefaultCredentials = false,
            Credentials = new NetworkCredential("your-email@gmail.com", "your-password"),
            EnableSsl = true
        };

        var message = new MailMessage("your-email@gmail.com", "recipient@example.com", "Test Subject", "Test Body");
        try
        {
            await client.SendMailAsync(message);
            Console.WriteLine("email wysłany");
        }
        catch (SmtpException ex)
        {
            Console.WriteLine($"błąd SMTP: {ex.Message}");
        }
        finally
        {
            message.Dispose();
            client.Dispose();
        }
        Console.WriteLine();
    }
}
