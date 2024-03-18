namespace Zad1 {
    public static class StringExtension
    {
        static bool isWhiteSpace(char c)
        {
            return c == ' ' || c == '.' || c == ',' || c == '?' || c == '!';
        }

        public static bool isPalindrome(this string str)
        {
            int i = 0;
            int j = str.Length - 1;
            while (i < j)
            {
                while (isWhiteSpace(str[i])) i++;
                while (isWhiteSpace(str[j])) j--;

                if (str[i] != str[j])
                    return false;
                
                i++;
                j--;
            }

            return true;
        }
    }

    public class Program1
    {
        public static void Main(string[] args)
        {
            string str = "Kobyła ma mały bok.";
            Console.WriteLine(str.isPalindrome());
            str = "kajak";
            Console.WriteLine(str.isPalindrome());
        }
    }
}