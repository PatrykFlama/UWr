using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;

namespace Zad1
{
    public class Calculate
    {
        public int statical(int x, int y)
        {
            return x + y;
        }

        public dynamic dynamical(dynamic x, dynamic y)
        {
            return x + y;
        }
    }
    
    public class Benchmark
    {
        public int N;
        public int x, y;
    }

    public class ComparisonBenchmark
    {
        [Benchmark]
        public int DoWork1_Concrete(int x, int y)
        {
            return x + y;
        }

        [Benchmark]
        public dynamic DoWork2_Dynamic(dynamic x, dynamic y)
        {
            return x + y;
        }
    }

    class Program1
    {
        public static void Main(string[] args)
        {
            var summary = BenchmarkRunner.Run<ComparisonBenchmark>();
        }
    }
}


