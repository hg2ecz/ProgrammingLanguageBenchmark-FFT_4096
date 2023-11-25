using System;
using CommandLine;

using static CSharpFftDemo.GlobalResourceManager;

namespace CSharpFftDemo;

public class Arguments
{
    [Option(shortName: 'd', longName: "dotnet-benchmark", Default = false,
        Required = false, HelpText = "Run the BenchmarkDotNet benchmark")]
    public bool DotnetBenchmark { get; set; }

    [Option(shortName: 'm', longName: "managed", Default = true,
        Required = false, HelpText = "Run the .NET managed benchmark")]
    public bool ManagedBenchmark { get; set; }

    [Option(shortName: 'n', longName: "native", Default = true,
        Required = false, HelpText = "Run the native (C-fast_double) benchmark")]
    public bool NativeBenchmark { get; set; }

    [Option(shortName: 'M', longName: "mathnet", Default = true,
        Required = false, HelpText = "Run the MathNet benchmark")]
    public bool MathNetBenchmark { get; set; }

    [Option(shortName: 'r', longName: "repeat", Default = 20000,
        Required = false, HelpText = "Number of iterations, e.g. 20000.")]
    public int FftRepeat { get; set; }

    [Option(shortName: 's', longName: "size", Default = 12,
        Required = false, HelpText = "Log2 of the buffer size, e.g. 12 for 4096 samples.")]
    public int Log2FftSize { get; set; }
}

public static class Benchmark
{
    public static int Main(string[] args)
    {
        return Parser.Default.ParseArguments<Arguments>(args)
            .MapResult((Arguments opts) =>
            {
                try
                {
                    FftMathNet.SetupMathNet();

                    Console.WriteLine($"Log2FftSize: {opts.Log2FftSize}, Repeat: {opts.FftRepeat}");

                    Params.Log2FftSize = opts.Log2FftSize;
                    Params.FftRepeat = opts.FftRepeat;

                    Benchmarks(
                        opts.DotnetBenchmark, opts.ManagedBenchmark,
                        opts.NativeBenchmark, opts.MathNetBenchmark);

                    return 0;
                }
                #pragma warning disable CA1031 // Do not catch general exception types
                catch (Exception e)
                {
                    Console.WriteLine(GetStringResource("UnhandledExceptionText")!, e.Message);
                    return -4;
                }
                #pragma warning restore CA1031 // Do not catch general exception types
            },
            errs => -1
        );
    }

    private static int Benchmarks(bool dotnetBenchmark, bool managedBenchmark, bool nativeBenchmark, bool mathNetBenchmark)
    {
        double? managedElapsedMillisecond = null;
        double? nativeElapsedMillisecond = null;
        double? mathNetElapsedMillisecond = null;

        if (dotnetBenchmark)
        {
            // Benchmark
            DotnetBenchmark.Calculate();
        }

        if (managedBenchmark)
        {
            // Warmup
            Console.ForegroundColor = ConsoleColor.Green;
            Console.WriteLine(GetStringResource("ManagedWarmupText"));
            Console.ForegroundColor = ConsoleColor.Gray;

            FftManaged.WarmUp(Params.Log2FftSize, Params.FftRepeat);

            // Benchmark
            Console.ForegroundColor = ConsoleColor.Yellow;
            Console.WriteLine(GetStringResource("ManagedText"));
            Console.ForegroundColor = ConsoleColor.Gray;

            managedElapsedMillisecond = FftManaged.Calculate(Params.Log2FftSize, Params.FftRepeat);
        }

        if (nativeBenchmark)
        {
            try
            {
                // Benchmark
                Console.ForegroundColor = ConsoleColor.Yellow;
                Console.WriteLine(GetStringResource("NativeText"));
                Console.ForegroundColor = ConsoleColor.Gray;

                nativeElapsedMillisecond = FftNative.Calculate(Params.Log2FftSize, Params.FftRepeat);
            }
            catch (DllNotFoundException e)
            {
                Console.WriteLine(GetStringResource("CanotRunNative")!, e.Message);
                Console.WriteLine(GetStringResource("HaveYouCompiledNative"));
                return 1;
            }
        }

        if (mathNetBenchmark)
        {
            // Warmup
            Console.ForegroundColor = ConsoleColor.Green;
            Console.WriteLine(GetStringResource("MathNetWarmupText"));
            Console.ForegroundColor = ConsoleColor.Gray;

            FftMathNet.WarmUp(Params.Log2FftSize, Params.FftRepeat);

            // Benchmark
            Console.ForegroundColor = ConsoleColor.Yellow;
            Console.WriteLine(GetStringResource("MathNetText"));
            Console.ForegroundColor = ConsoleColor.Gray;

            mathNetElapsedMillisecond = FftMathNet.Calculate(Params.Log2FftSize, Params.FftRepeat);
        }

        if (managedElapsedMillisecond.HasValue && nativeElapsedMillisecond.HasValue)
        {
            Console.ForegroundColor = ConsoleColor.Green;
            Console.WriteLine($"Native Ratio: {managedElapsedMillisecond / nativeElapsedMillisecond:0.####}");
            Console.WriteLine($"Native Diff%: {managedElapsedMillisecond / nativeElapsedMillisecond - 1:0.##%}");
            Console.ForegroundColor = ConsoleColor.Gray;
        }

        if (managedElapsedMillisecond.HasValue && mathNetElapsedMillisecond.HasValue)
        {
            Console.ForegroundColor = ConsoleColor.Green;
            Console.WriteLine($"\nMathNet Ratio: {mathNetElapsedMillisecond / nativeElapsedMillisecond:0.####}");
            Console.WriteLine($"MathNet Diff%: {mathNetElapsedMillisecond / nativeElapsedMillisecond - 1:0.##%}");
            Console.ForegroundColor = ConsoleColor.Gray;
        }

        return 0;
    }
}
