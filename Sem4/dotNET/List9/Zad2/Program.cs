using System;
using System.Threading;

/*
problem golibrody / cigarette smokers problem
https://en.wikipedia.org/wiki/Cigarette_smokers_problem
https://w3.cs.jmu.edu/kirkpams/OpenCSF/Books/csf/html/CigSmokers.html
*/

public class Program
{
    static Semaphore agentSem = new Semaphore(1, 1); 
    static Semaphore tobacco = new Semaphore(0, 1);
    static Semaphore paper = new Semaphore(0, 1);
    static Semaphore matches = new Semaphore(0, 1);

    // agent dostarczający składniki
    static void Agent()
    {
        while (true)
        {
            agentSem.WaitOne(); // czeka na agenta
            Thread.Sleep(1000); // symulacja czasu dostarczania składników

            Random rnd = new Random();
            int itemProvided = rnd.Next(0, 3);

            if (itemProvided == 0)
            {
                Console.WriteLine("agent dostarcza tytoń i papier");
                tobacco.Release();
                paper.Release();
            }
            else if (itemProvided == 1)
            {
                Console.WriteLine("agent dostarcza tytoń i zapałki");
                tobacco.Release();
                matches.Release();
            }
            else
            {
                Console.WriteLine("agent dostarcza papier i zapałki");
                paper.Release();
                matches.Release();
            }
        }
    }

    // palacze biorą składniki i palą
    static void SmokerWithTobacco()
    {
        while (true)
        {
            tobacco.WaitOne();
            paper.WaitOne();
            Console.WriteLine("palacz z tytoniem rozpoczyna palenie");
            Thread.Sleep(1000);
            Console.WriteLine("palacz z tytoniem kończy palenie");
            agentSem.Release(); // informuje agenta że można dostarczyć składniki
        }
    }

    static void SmokerWithPaper()
    {
        while (true)
        {
            tobacco.WaitOne();
            matches.WaitOne();
            Console.WriteLine("palacz z papierem rozpoczyna palenie.");
            Thread.Sleep(1000);
            Console.WriteLine("palacz z papierem kończy palenie");
            agentSem.Release();
        }
    }

    static void SmokerWithMatches()
    {
        while (true)
        {
            paper.WaitOne();
            matches.WaitOne();
            Console.WriteLine("palacz z zapałkami rozpoczyna palenie");
            Thread.Sleep(1000);
            Console.WriteLine("palacz z zapałkami kończy palenie");
            agentSem.Release();
        }
    }

    public static void Main(string[] args)
    {
        Thread agentThread = new Thread(Agent);
        Thread smokerTobaccoThread = new Thread(SmokerWithTobacco);
        Thread smokerPaperThread = new Thread(SmokerWithPaper);
        Thread smokerMatchesThread = new Thread(SmokerWithMatches);

        agentThread.Start();
        smokerTobaccoThread.Start();
        smokerPaperThread.Start();
        smokerMatchesThread.Start();

        agentThread.Join();
        smokerTobaccoThread.Join();
        smokerPaperThread.Join();
        smokerMatchesThread.Join();
    }
}
