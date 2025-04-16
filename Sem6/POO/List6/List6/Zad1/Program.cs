namespace Zad1
{
    public interface ILogger
    {
        void Log(string Message);
    }

    public class NullLogger : ILogger
    {
        public void Log(string message) { }
    }

    public class ConsoleLogger : ILogger
    {
        public void Log(string message)
        {
            Console.WriteLine($"[Console] {message}");
        }
    }

    public class FileLogger : ILogger
    {
        private readonly string _filePath;

        public FileLogger(string filePath)
        {
            _filePath = filePath;
        }

        public void Log(string message)
        {
            try
            {
                File.AppendAllText(_filePath, message + Environment.NewLine);
            }
            catch(Exception ex)
            {
                Console.WriteLine($"[Error writing to file: {_filePath}] {ex.Message}");
            }
        }
    }

    public enum LogType { None, Console, File }

    public class LoggerFactory
    {
        private static readonly LoggerFactory _instance = new LoggerFactory();


        public static LoggerFactory Instance => _instance;

        public ILogger GetLogger(LogType LogType, string parameters = null)
        {
            switch (LogType)
            {
                case LogType.Console:
                    return new ConsoleLogger();
                case LogType.File:
                    if(!string.IsNullOrEmpty(parameters))
                        return new FileLogger(parameters);
                    else
                        throw new ArgumentException("File path must be provided for File logger.");
                default:
                    return new NullLogger();
            }
        }
    }


}
