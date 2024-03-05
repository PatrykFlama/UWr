namespace Zad6
{
    class Program
    {
        static void Main(string[] args)
        {

        }
    }

    class Vector
    {
        int x, y;

        Vector(int _x, int _y)
        {
            x = _x;
            y = _y;
        }

        public static Vector operator +(Vector a, Vector b) => new Vector(a.x + b.x, a.y + b.y);
        public static Vector operator -(Vector a, Vector b) => new Vector(a.x - b.x, a.y - b.y);
    }
}