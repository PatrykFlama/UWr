using System.Net.Mail;
using System.Net;

namespace Zad1
{
    internal class Program
    {
        //static void Main(string[] args) { }

        public class SmtpFacade
        {
            private readonly SmtpClient _smtpClient;

            public SmtpFacade(string host, int port, string username, string password, bool enableSsl = true)
            {
                _smtpClient = new SmtpClient(host, port)
                {
                    EnableSsl = enableSsl,
                    Credentials = new NetworkCredential(username, password)
                };
            }

            public void Send(string from, string to, string subject, string body, Stream attachment = null, string attachmentMimeType = null)
            {
                using(var message = new MailMessage(from, to, subject, body))
                {
                    if(attachment != null && attachmentMimeType != null)
                    {
                        var att = new Attachment(attachment, "attachment", attachmentMimeType);
                        message.Attachments.Add(att);
                    }

                    _smtpClient.Send(message);
                }
            }
        }

        public interface ISmtpClient
        {
            void Send(MailMessage message);
        }

        public class RealSmtpClient : ISmtpClient
        {
            private readonly SmtpClient _client;

            public RealSmtpClient(SmtpClient client)
            {
                _client = client;
            }

            public void Send(MailMessage message)
            {
                _client.Send(message);
            }
        }

        public class TestableSmtpFacade
        {
            private readonly ISmtpClient _smtpClient;

            public TestableSmtpFacade(ISmtpClient smtpClient)
            {
                _smtpClient = smtpClient;
            }

            public void Send(string from, string to, string subject, string body, Stream attachment = null, string attachmentMimeType = null)
            {
                using(var message = new MailMessage(from, to, subject, body))
                {
                    if(attachment != null && attachmentMimeType != null)
                    {
                        var att = new Attachment(attachment, "attachment", attachmentMimeType);
                        message.Attachments.Add(att);
                    }

                    _smtpClient.Send(message);
                }
            }
        }
    }
}
