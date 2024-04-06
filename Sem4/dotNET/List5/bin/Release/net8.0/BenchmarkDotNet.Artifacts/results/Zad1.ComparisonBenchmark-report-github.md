```

BenchmarkDotNet v0.13.12, Windows 11 (10.0.22631.3374/23H2/2023Update/SunValley3)
AMD Ryzen 5 5600U with Radeon Graphics, 1 CPU, 12 logical and 6 physical cores
.NET SDK 8.0.200
  [Host]     : .NET 8.0.2 (8.0.224.6711), X64 RyuJIT AVX2 [AttachedDebugger]
  DefaultJob : .NET 8.0.2 (8.0.224.6711), X64 RyuJIT AVX2


```
| Method                     | Mean        | Error     | StdDev    | Median      |
|--------------------------- |------------:|----------:|----------:|------------:|
| Benchmark_Concrete_Simple  |   0.0165 ns | 0.0142 ns | 0.0133 ns |   0.0168 ns |
| Benchmark_Dynamic_Simple   |  14.8442 ns | 0.3487 ns | 0.8355 ns |  14.8632 ns |
| Benchmark_Concrete_Complex |   0.4909 ns | 0.0151 ns | 0.0134 ns |   0.4879 ns |
| Benchmark_Dynamic_Complex  | 113.3088 ns | 2.3285 ns | 6.8657 ns | 111.4343 ns |
