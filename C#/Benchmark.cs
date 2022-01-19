using System;
using System.CommandLine;
using System.IO;

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
        double? managedElapsedMillisecond = null;
        double? nativeElapsedMillisecond = null;

        if (dotnetBenchmark)
        {
            DotnetBenchmark.Calculate();
        }

        if (managedBenchmark)
        {
            Console.ForegroundColor = ConsoleColor.Green;
            Console.WriteLine("---- MANAGED ----");
            Console.ForegroundColor = ConsoleColor.Gray;

            managedElapsedMillisecond = FftManaged.Calculate(Params.Log2FftSize, Params.FftRepeat);
        }

        if (nativeBenchmark)
        {
            try
            {
                Console.ForegroundColor = ConsoleColor.Green;
                Console.WriteLine("---- NATIVE ----");
                Console.ForegroundColor = ConsoleColor.Gray;

                nativeElapsedMillisecond = FftNative.Calculate(Params.Log2FftSize, Params.FftRepeat);
            }
            catch (Exception e)
            {
                Console.WriteLine($"Can not run native method: {e.Message}");
                Console.WriteLine("Have you successfully compiled the project in ../C-tests/C-fast_double/?");
                return 1;
            }
        }

        if (managedElapsedMillisecond.HasValue && nativeElapsedMillisecond.HasValue)
        {
            Console.ForegroundColor = ConsoleColor.Green;
            Console.WriteLine("\nRatio: {0:0.4}", (double)managedElapsedMillisecond / nativeElapsedMillisecond);
            Console.ForegroundColor = ConsoleColor.Gray;
        }

        return 0;
    }
}
