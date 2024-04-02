using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;

namespace Zad1
{
    public class ComparisonBenchmark
    {
        [Params(5, 10)]
        public int x;

        [Params(10, 20)]
        public int y;



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

            

            //int x = 5;
            //int y = 10;
            //dynamic dynamicX = 5;
            //dynamic dynamicY = 10;

            //var summary1 = BenchmarkRunner.Run<ComparisonBenchmark>(new BenchmarkDotNet.Configs.DefaultConfig().WithArguments(new[] { "--x", x.ToString(), "--y", y.ToString() }));
            //var summary2 = BenchmarkRunner.Run<ComparisonBenchmark>(new BenchmarkDotNet.Configs.DefaultConfig().WithArguments(new[] { "--dynamicX", dynamicX.ToString(), "--dynamicY", dynamicY.ToString() }));
        }
    }
}


