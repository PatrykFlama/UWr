namespace Libone
{
    public class TheGreatAdder
    {
        int a, b;

        public TheGreatAdder() : this(0, 0) { }
        public TheGreatAdder(int _a, int _b)
        {
            a = _a;
            b = _b;
        }

        public int add()
        {
            return a + b;
        }
        public void cha(int _a)
        {
            a = _a;
        }
        public void chb(int _b)
        {
            b = _b;
        }
    }
}
