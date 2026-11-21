using System;
using System.Numerics;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Columns;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Loggers;
using BenchmarkDotNet.Running;

namespace FftBenchmark;

/// <summary>
/// BenchmarkDotNet comparison of the three implementations. Not sealed: BenchmarkDotNet
/// generates a derived type.
/// </summary>
[MinColumn, MaxColumn, MeanColumn, MedianColumn]
[MemoryDiagnoser]
[DisassemblyDiagnoser]
public class DotnetBenchmark : IDisposable
{
    private sealed class Config : ManualConfig
    {
        public Config()
        {
            AddLogger(ConsoleLogger.Default);
            AddColumnProvider(DefaultColumnProviders.Instance);
            WithOptions(ConfigOptions.DisableOptimizationsValidator);
        }
    }

    // BenchmarkDotNet runs the benchmarks in a child process, so the size is passed as a [Params] value.
    [Params(12)]
    public int Log2FftSize { get; set; }

    private Complex[] xyIn = [];
    private Complex[] xyOut = [];
    private NativeRust? rust;

    public static void Run(int log2FftSize)
    {
        Measurement.WriteTitle("BENCHMARK.NET", ConsoleColor.Yellow);

        var config = new Config();

        if (log2FftSize != 12)
        {
            Console.WriteLine("Note: BenchmarkDotNet always uses Log2FftSize = 12 (see [Params]).");
        }

        _ = BenchmarkRunner.Run<DotnetBenchmark>(config);
    }

    [GlobalSetup]
    public void Setup()
    {
        int size = 1 << Log2FftSize;
        xyIn = Measurement.CreateTestInput(size);
        xyOut = new Complex[size];
        rust = new FftBenchmark.NativeRust();
    }

    [GlobalCleanup]
    public void Cleanup() => Dispose();

    [Benchmark]
    public void Managed() => FftManaged.Calculate(Log2FftSize, xyIn, xyOut);

    [Benchmark(Baseline = true)]
    public void NativeC() => FftBenchmark.NativeC.Calculate(Log2FftSize, xyIn, xyOut);

    [Benchmark]
    public void NativeRust() => rust!.Calculate(Log2FftSize, xyIn, xyOut);

    public void Dispose()
    {
        Dispose(true);
        GC.SuppressFinalize(this);
    }

    protected virtual void Dispose(bool disposing)
    {
        if (disposing)
        {
            rust?.Dispose();
            rust = null;
        }
    }
}
