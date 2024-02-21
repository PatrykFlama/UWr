namespace Libduo
{
    public class Textify
    {
        public static string Exec(int n)
        {
            return Exec(n.ToString());
        }
        public static string Exec(string s)
        {
            string ns = "";
            foreach (char c in s)
            {
                ns += "|" + c;
            }
            ns += '|';
            return ns;
        }
    }
}
