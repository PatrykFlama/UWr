    Zad1 z = new Zad1();
    z.main();

    class Zad1
    {
        bool test_condition(int n)      // check wheather test condition fits given number
        {
        int m = n;
            int sum = 0;
            while(m > 0)        // iterate over digits of number
            {
                int d = m % 10;
                m /= 10;

                sum += d;       // calc sum of digits
                if (d != 0 && n % d != 0) return false;     // if nonzero ditig of number is not dividing it
            }

            if (n % sum != 0) return false;     // if sum of digits of number is not dividing it
            return true;
        }

        public void main()
        {
            for(int i = 1; i <= 1e5; i++)       // loop over all tested numbers
            {
                if (test_condition(i))
                {
                    Console.WriteLine(i);
                }
            }
        }
    }




