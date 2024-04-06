using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;

namespace Zad1
{
    public class Worker
    {
        public int Concrete(int x, int y)
        {
            return x + y;
        }

        public dynamic Dynamic(dynamic x, dynamic y)
        {
            return x + y;
        }
    }

    public class ComparisonBenchmark
    {
        public int N;
        public int x, y;
        Worker worker = new Worker();

        [Benchmark]
        public int Benchmark_Concrete()
        {
            return worker.Concrete(x, y);
        }

        [Benchmark]
        public dynamic Benchmark_Dynamic()
        {
            return worker.Dynamic(x, y);
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


