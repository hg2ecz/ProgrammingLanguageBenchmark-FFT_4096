using System;
using System.CommandLine;
using System.Diagnostics;
using System.IO;
using System.Numerics;
using BenchmarkDotNet.Columns;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Loggers;
using BenchmarkDotNet.Running;
using BenchmarkDotNet.Validators;

namespace CSharpFftDemo;

public static class Benchmark
{
    private static int Main(string[] args)
    {
        var dotnetBenchmarkOption = new Option<bool>(
            new [] {
                "-d",
                "--dotnet-benchmark"
            },
            getDefaultValue: () => false,
            "Run the BenchmarkDotNet benchmark");

        var managedBenchmarkOption = new Option<bool>(
            new [] {
                "-m",
                "--managed"
            },
            getDefaultValue: () => true,
            "Run the .NET managed benchmark");

        var nativeBenchmarkOption = new Option<bool>(
            new [] {
                "-n",
                "--native"
            },
            getDefaultValue: () => File.Exists($"{AppDomain.CurrentDomain.BaseDirectory}{Path.DirectorySeparatorChar}libfft.so"),
            "Run the native (C-fast_double) benchmark");

        var rootCommand = new RootCommand
        {
            dotnetBenchmarkOption,
            managedBenchmarkOption,
            nativeBenchmarkOption
        };

        rootCommand.Description = "FFT Benchmark from Zsolt Krüpl.";

        rootCommand.SetHandler((bool dotnetBenchmark, bool  managedBenchmark, bool nativeBenchmark) =>
            Benchmarks(dotnetBenchmark, managedBenchmark, nativeBenchmark),
            dotnetBenchmarkOption, managedBenchmarkOption, nativeBenchmarkOption);

        return rootCommand.Invoke(args);
    }

    private static int Benchmarks(bool dotnetBenchmark, bool managedBenchmark, bool nativeBenchmark)
    {
        if (dotnetBenchmark)
        {
            DotnetBenchmark();
        }

        if (managedBenchmark)
        {
            Console.ForegroundColor = ConsoleColor.Green;
            Console.WriteLine("---- MANAGED ----");
            Console.ForegroundColor = ConsoleColor.Gray;

            Managed(Params.Log2FftSize, Params.FftRepeat);
        }

        if (nativeBenchmark)
        {
            try
            {
                Console.ForegroundColor = ConsoleColor.Green;
                Console.WriteLine("---- NATIVE ----");
                Console.ForegroundColor = ConsoleColor.Gray;

                FftNative.Native(Params.Log2FftSize, Params.FftRepeat);
            }
            catch (Exception e)
            {
                Console.WriteLine($"Can not run native method: {e.Message}");
                Console.WriteLine("Have you successfully compiled the project in ../C-tests/C-fast_double/?");
                return 1;
            }
        }

        return 0;
    }

    private static void DotnetBenchmark()
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

    private static void Managed(int log2FftSize, int fftRepeat)
    {
        int i;
        int size = 1 << log2FftSize;
        Complex[] xy = new Complex[size];
        Complex[] xy_out = new Complex[xy.Length];

        for (i = 0; i < size / 2; i++)
            xy[i] = new Complex(1.0, 0.0);

        for (i = size / 2; i < size; i++)
            xy[i] = new Complex(-1.0, 0.0);

        // JIT warm up ... possible give more speed
        for (i = 0; i < fftRepeat; i++)
        {
            Fft.Calculate(log2FftSize, xy, xy_out);
        }

        // FFT
        var stopwatch = Stopwatch.StartNew();

        for (i = 0; i < fftRepeat; i++)
        {
            Fft.Calculate(log2FftSize, xy, xy_out);
        }

        stopwatch.Stop();

        Console.WriteLine($"Total ({fftRepeat}): {stopwatch.ElapsedMilliseconds}");

        var tpp = stopwatch.ElapsedMilliseconds / (float)fftRepeat;

        Console.WriteLine($"{fftRepeat} piece(s) of {1 << log2FftSize} pt FFT;  {tpp} ms/piece\n");

        for (i = 0; i < 6; i++)
        {
            Console.WriteLine("{0}\t{1}", i, xy_out[i]);
        }
    }
}
