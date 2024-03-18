public class Program
{
    public static void Main(string[] args)
    {
        List<int> list = new() { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

        list = list.ConvertAll(delegate (int i) { return i + 10; });
        list.ForEach(delegate (int i) { Console.Write(i); Console.Write(' '); });
        Console.Write('\n');
        
        list.RemoveAll(delegate (int i) { return i > 15; });
        list.ForEach(delegate (int i) { Console.Write(i); Console.Write(' '); });
        Console.Write('\n');
        
        list.Sort(delegate (int a, int b)
        {
            return a < b ? 1 : -1;
        });
        list.ForEach(delegate (int i) { Console.Write(i); Console.Write(' '); });
        Console.Write('\n');
    }
}