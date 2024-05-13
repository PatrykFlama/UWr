using System;
using System.IO;
using System.Reflection;

public static class ResourceHelper
{
    public static void ExtractResourceToFile(string resourceName, string outputPath)
    {
        // pobierz przestrzeń nazw
        Assembly assembly = Assembly.GetExecutingAssembly();

        // lista wszystkich zasobów w zestawie
        string[] allResourceNames = assembly.GetManifestResourceNames();

        // sprawdź czy podana nazwa zasobu istnieje w zestawie
        bool resourceExists = false;
        foreach (string existingResourceName in allResourceNames)
        {
            if (string.Equals(existingResourceName, resourceName, StringComparison.OrdinalIgnoreCase))
            {
                resourceExists = true;
                break;
            }
        }

        if (!resourceExists)
        {
            throw new ArgumentException($"resource '{resourceName}' not found in the assembly");
        }

        // otwórz plik
        using (Stream resourceStream = assembly.GetManifestResourceStream(resourceName))
        {
            if (resourceStream == null)
            {
                throw new InvalidOperationException($"failed to extract resource '{resourceName}'");
            }

            // zapisz plik
            using (FileStream fileStream = new FileStream(outputPath, FileMode.Create))
            {
                resourceStream.CopyTo(fileStream);
            }
        }
    }

    public static void PrintFileContent(string filePath)
    {
        try
        {
            string fileContent = File.ReadAllText(filePath);

            Console.WriteLine($"Zawartość '{Path.GetFileName(filePath)}':");
            Console.WriteLine(fileContent);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"błąd podczas odczytu pliku: {ex.Message}");
        }
    }




    public class Program
    {
        static void Main()
        {

            try
            {
                // nazwa pliku wraz z nazwą przestrzeni nazw
                string resourceName = "Zad6.plik.txt";

                // docelowa ścieżka do pliku
                string outputPath = "./wczytanyplik.txt";

                ResourceHelper.ExtractResourceToFile(resourceName, outputPath);

                Console.WriteLine("znaleziono plik");

                ResourceHelper.PrintFileContent(outputPath);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"błąd podczas wydobywania zasobu: {ex.Message}");
            }

        }
    }
}


