using System;
using System.IO;
using Xunit;
using Zad4v2;

namespace Zad4v2Tests
{
    public class PersonRegistryNotifierBridgeTests
    {
        [Fact]
        public void XmlRegistry_EmailNotifier_ShouldSendEmails()
        {
            var notifier = new EmailNotifier();
            var registry = new XmlPersonRegistry(notifier);

            using var sw = new StringWriter();
            Console.SetOut(sw);

            registry.NotifyPersons();

            string output = sw.ToString();
            Assert.Contains("Email sent to Anna", output);
            Assert.Contains("Email sent to Jan", output);
        }

        [Fact]
        public void DbRegistry_SmsNotifier_ShouldSendSms()
        {
            var notifier = new SmsNotifier();
            var registry = new DbPersonRegistry(notifier);

            using var sw = new StringWriter();
            Console.SetOut(sw);

            registry.NotifyPersons();

            string output = sw.ToString();
            Assert.Contains("SMS sent to Kasia", output);
            Assert.Contains("SMS sent to Tomek", output);
        }
    }

}
