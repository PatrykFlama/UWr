Zad1 z = new Zad1();
z.main();

class Zad1
{
    bool test_condition(int n)
    {
        int m = n;
        int sum = 0;
        while(m > 0)
        {
            int d = m % 10;
            m /= 10;

            sum += d;
            if (d != 0 && n % d != 0) return false;
        }

        if (n % sum != 0) return false;
        return true;
    }

    public void main()
    {
        for(int i = 1; i <= 1e5; i++)
        {
            if (test_condition(i))
            {
                Console.WriteLine(i);
            }
        }
    }
}




