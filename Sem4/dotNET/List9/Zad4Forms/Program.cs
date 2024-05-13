using System.Globalization;

namespace Zad4Forms
{
    internal static class Program
    {
        /// <summary>
        ///  The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            // To customize application configuration such as set high DPI settings or default font,
            // see https://aka.ms/applicationconfiguration.
            ApplicationConfiguration.Initialize();
            Application.Run(new Form1());

            Languify();
        }

        static void Languify()
        {
            string[] cultures = { "en", "de", "fr", "ru", "ar", "cs", "pl" };

            foreach (string culture in cultures)
            {
                CultureInfo cultureInfo = new CultureInfo(culture);

                string monthsFull = string.Join(", ", cultureInfo.DateTimeFormat.MonthNames);
                string monthsShort = string.Join(", ", cultureInfo.DateTimeFormat.AbbreviatedMonthNames);

                string daysFull = string.Join(", ", cultureInfo.DateTimeFormat.DayNames);
                string daysShort = string.Join(", ", cultureInfo.DateTimeFormat.AbbreviatedDayNames);

                string currentDate = DateTime.Now.ToString("D", cultureInfo);


                string infoMessage = $"nazwa kultury: {cultureInfo.DisplayName}\n" +
                                        $"miesi¹ce pe³ne: {monthsFull}\n" +
                                        $"miesi¹ce krótkie: {monthsShort}\n" +
                                        $"dni pe³ne: {daysFull}\n" +
                                        $"dni którtkie: {daysShort}\n" +
                                        $"bie¿¹ca data: {currentDate}";

                if (culture == "ar" || culture == "ru")
                {
                    MessageBox.Show(infoMessage, "Informacje o dacie", MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
                else
                {
                    Console.WriteLine(infoMessage);
                }
            }

        }
    }
}