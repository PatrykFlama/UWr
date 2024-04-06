using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;

namespace Zad1
{
    public class Worker
    {
        public int Concrete_Simple(int x, int y)
        {
            return x + y;
        }

        public dynamic Dynamic_Simple(dynamic x, dynamic y)
        {
            return x + y;
        }


        public int Concrete_Complex(int x, int y)
        {
            return (x + y * x + y * x + y * x + y * x + y * x + y * x + y * x + y);
        }

        public dynamic Dynamic_Complex(dynamic x, dynamic y)
        {
            return (x + y*x + y*x + y*x + y*x + y*x + y*x + y*x + y);
        }
    }

    public class ComparisonBenchmark
    {
        public int N;
        public int x, y;
        Worker worker = new Worker();

        [Benchmark]
        public int Benchmark_Concrete_Simple()
        {
            return worker.Concrete_Simple(x, y);
        }

        [Benchmark]
        public dynamic Benchmark_Dynamic_Simple()
        {
            return worker.Dynamic_Simple(x, y);
        }

        [Benchmark]
        public int Benchmark_Concrete_Complex()
        {
            return worker.Concrete_Complex(x, y);
        }

        [Benchmark]
        public dynamic Benchmark_Dynamic_Complex()
        {
            return worker.Dynamic_Complex(x, y);
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

/*
| Method                     | Mean        | Error     | StdDev    | Median      |
|--------------------------- |------------:|----------:|----------:|------------:|
| Benchmark_Concrete_Simple  |   0.0165 ns | 0.0142 ns | 0.0133 ns |   0.0168 ns |
| Benchmark_Dynamic_Simple   |  14.8442 ns | 0.3487 ns | 0.8355 ns |  14.8632 ns |
| Benchmark_Concrete_Complex |   0.4909 ns | 0.0151 ns | 0.0134 ns |   0.4879 ns |
| Benchmark_Dynamic_Complex  | 113.3088 ns | 2.3285 ns | 6.8657 ns | 111.4343 ns |
*/


