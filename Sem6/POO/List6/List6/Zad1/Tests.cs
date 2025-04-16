using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Zad1;
using System;
using System.IO;
using Xunit;

namespace Zad1Tests
{

    public class zad1Tests
    {
        [Fact]
        public void NullLogger_ShouldNotThrowOrLog()
        {
            ILogger logger = new NullLogger();

            var exception = Record.Exception(() => logger.Log("This should do nothing"));
            Assert.Null(exception);
        }

        [Fact]
        public void ConsoleLogger_ShouldWriteToConsole()
        {
            ILogger logger = new ConsoleLogger();
            using var sw = new StringWriter();
            Console.SetOut(sw);

            string testMessage = "Hello console";
            logger.Log(testMessage);

            string consoleOutput = sw.ToString();
            Assert.Contains(testMessage, consoleOutput);
        }

        [Fact]
        public void FileLogger_ShouldWriteToFile()
        {
            string path = Path.GetTempFileName();
            ILogger logger = new FileLogger(path);

            string message = "Hello world";

            logger.Log(message);

            string result = File.ReadAllText(path);
            Assert.Contains(message, result);

            File.Delete(path);
        }

        [Fact]
        public void LoggerFactory_ShouldReturn_NullLogger()
        {
            var factory = LoggerFactory.Instance;

            var logger = factory.GetLogger(LogType.None);

            Assert.IsType<NullLogger>(logger);
        }

        [Fact]
        public void LoggerFactory_ShouldReturn_ConsoleLogger()
        {
            var factory = LoggerFactory.Instance;

            var logger = factory.GetLogger(LogType.Console);

            Assert.IsType<ConsoleLogger>(logger);
        }

        [Fact]
        public void LoggerFactory_ShouldReturn_FileLogger_WithPath()
        {
            var factory = LoggerFactory.Instance;

            string tempFile = Path.GetTempFileName();
            var logger = factory.GetLogger(LogType.File, tempFile);

            Assert.IsType<FileLogger>(logger);

            File.Delete(tempFile);
        }

        [Fact]
        public void LoggerFactory_FileLogger_WithoutPath_ShouldThrow()
        {
            var factory = LoggerFactory.Instance;

            Assert.Throws<ArgumentException>(() =>
            {
                factory.GetLogger(LogType.File);
            });
        }
    }
}
