using System;
using System.IO;
using System.Net.Mail;
using System.Text;
using Moq;
using Xunit;
using System.Linq;
using Zad1;

namespace Zad1Tests
{
    public interface ISmtpClient
    {
        void Send(MailMessage message);
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

    public class SmtpFacadeTests
    {
        [Fact]
        public void Send_ShouldCallSmtpClientSend_WithCorrectData()
        {
            // Arrange
            var smtpClientMock = new Mock<ISmtpClient>();

            var facade = new TestableSmtpFacade(smtpClientMock.Object);

            string from = "from@example.com";
            string to = "to@example.com";
            string subject = "Hello";
            string body = "This is a test";

            // Act
            facade.Send(from, to, subject, body);

            // Assert
            smtpClientMock.Verify(s => s.Send(It.Is<MailMessage>(
                m => m.From.Address == from &&
                     m.To[0].Address == to &&
                     m.Subject == subject &&
                     m.Body == body
            )), Times.Once);
        }

        [Fact]
        public void Send_ShouldIncludeAttachment_WhenProvided()
        {
            // Arrange
            var smtpClientMock = new Mock<ISmtpClient>();

            MailMessage capturedMessage = null;

            smtpClientMock
                .Setup(s => s.Send(It.IsAny<MailMessage>()))
                .Callback<MailMessage>(msg =>
                {
                    // Skopiuj dane zanim MailMessage zostanie zniszczony
                    capturedMessage = new MailMessage(msg.From.Address, msg.To.First().Address, msg.Subject, msg.Body);
                    foreach(var att in msg.Attachments)
                    {
                        using var ms = new MemoryStream();
                        att.ContentStream.CopyTo(ms);
                        ms.Position = 0;
                        capturedMessage.Attachments.Add(new Attachment(ms, att.Name, att.ContentType.MediaType));
                    }
                });

            var facade = new TestableSmtpFacade(smtpClientMock.Object);

            var stream = new MemoryStream(Encoding.UTF8.GetBytes("hello world"));
            string mimeType = "text/plain";

            // Act
            facade.Send("from@example.com", "to@example.com", "Subject", "Body", stream, mimeType);

            // Assert
            Assert.NotNull(capturedMessage);
            Assert.Single(capturedMessage.Attachments);
            Assert.Equal(mimeType, capturedMessage.Attachments[0].ContentType.MediaType);
        }
    }
}