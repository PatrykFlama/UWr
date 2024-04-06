```

BenchmarkDotNet v0.13.12, Windows 11 (10.0.22631.3374/23H2/2023Update/SunValley3)
AMD Ryzen 5 5600U with Radeon Graphics, 1 CPU, 12 logical and 6 physical cores
.NET SDK 8.0.200
  [Host]     : .NET 8.0.2 (8.0.224.6711), X64 RyuJIT AVX2 [AttachedDebugger]
  DefaultJob : .NET 8.0.2 (8.0.224.6711), X64 RyuJIT AVX2


```
| Method             | Mean       | Error     | StdDev    |
|------------------- |-----------:|----------:|----------:|
| Benchmark_Concrete |  0.0895 ns | 0.0272 ns | 0.0530 ns |
| Benchmark_Dynamic  | 17.6808 ns | 0.3456 ns | 0.8147 ns |
