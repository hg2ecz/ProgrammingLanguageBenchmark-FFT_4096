using System;
using System.Numerics;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Columns;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Loggers;
using BenchmarkDotNet.Running;
using BenchmarkDotNet.Validators;

namespace CSharpFftDemo;

[MarkdownExporterAttribute.GitHub]
[MinColumn, MaxColumn]
internal class DotnetBenchmark
{
    private const int size = 1 << Params.Log2FftSize;
    readonly Complex[] xyManaged = new Complex[size];
    readonly Complex[] xyOutManaged = new Complex[size];

    private readonly FftNative.DoubleComplex[] xyNative = new FftNative.DoubleComplex[size];
    private readonly FftNative.DoubleComplex[] xyOutNative = new FftNative.DoubleComplex[size];

    public static void Calculate()
    {
        Console.ForegroundColor = ConsoleColor.Green;
        Console.WriteLine("---- Benchmark.NET ----");
        Console.ForegroundColor = ConsoleColor.Gray;

        var config = new ManualConfig()
            .WithOptions(ConfigOptions.DisableOptimizationsValidator)
            .AddValidator(JitOptimizationsValidator.DontFailOnError)
            .AddLogger(ConsoleLogger.Default)
            .AddColumnProvider(DefaultColumnProviders.Instance);

        BenchmarkRunner.Run<DotnetBenchmark>(config);
    }

    public DotnetBenchmark()
    {
        int i;

        for (i = 0; i < size / 2; i++)
            xyManaged[i] = new Complex(1.0, 0.0);

        for (i = size / 2; i < size; i++)
            xyManaged[i] = new Complex(-1.0, 0.0);

        for (i = 0; i < size / 2; i++)
            xyNative[i] = new FftNative.DoubleComplex(1.0f, 0.0f);

        for (i = size / 2; i < size; i++)
            xyNative[i] = new FftNative.DoubleComplex(-1.0f, 0.0f);
    }

    [Benchmark]
    public void Managed() => Fft.Calculate(Params.Log2FftSize, xyManaged, xyOutManaged);

    [Benchmark]
    public void Native() => FftNative.fft(Params.Log2FftSize, xyNative, xyOutNative);
}
