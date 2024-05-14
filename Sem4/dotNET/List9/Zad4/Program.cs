using System.Globalization;
using System.Windows.Forms;

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
                            $"miesiące pełne: {monthsFull}\n" +
                            $"miesiące krótkie: {monthsShort}\n" +
                            $"dni pełne: {daysFull}\n" +
                            $"dni którtkie: {daysShort}\n" +
                            $"bieżąca data: {currentDate}";

    if (culture == "ar" || culture == "ru")
    {
        MessageBox.Show(infoMessage, "Informacje o dacie", MessageBoxButtons.OK, MessageBoxIcon.Information);
    }
    else
    {
        Console.WriteLine(infoMessage);
    }

    //Console.WriteLine(infoMessage);
}
