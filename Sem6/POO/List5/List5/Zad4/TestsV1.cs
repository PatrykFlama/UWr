using System;
using System.Collections.Generic;
using System.IO;
using Xunit;
using Zad4v1;

namespace Zad4v1Tests
{ 
    public class PersonRegistryBridgeTests
    {
        [Fact]
        public void EmailRegistry_XmlSource_ShouldSendEmails()
        {
            var source = new XmlPersonSource();
            var registry = new EmailRegistry(source);

            using var sw = new StringWriter();
            Console.SetOut(sw);

            registry.NotifyPersons();

            string output = sw.ToString();
            Assert.Contains("Email sent to Anna", output);
            Assert.Contains("Email sent to Jan", output);
        }

        [Fact]
        public void SmsRegistry_DbSource_ShouldSendSms()
        {
            var source = new DbPersonSource();
            var registry = new SmsRegistry(source);

            using var sw = new StringWriter();
            Console.SetOut(sw);

            registry.NotifyPersons();

            string output = sw.ToString();
            Assert.Contains("SMS sent to Kasia", output);
            Assert.Contains("SMS sent to Tomek", output);
        }
    }
}