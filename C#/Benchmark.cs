using System;
using System.Globalization;
using System.Reflection;
using System.Resources;
using System.Threading.Tasks;
using CommandLine;

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
    private static ResourceManager resourceManager = new ResourceManager("FftBenchmark.Resources.Strings", Assembly.GetExecutingAssembly());

    public static async Task<int> Main(string[] args) => 
        await Parser.Default.ParseArguments<Arguments>(args)
            .MapResult(async (Arguments opts) =>
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
                catch (Exception e)
                {
                    Console.WriteLine("Unhandled exception: " + e.Message);
                    return -3; // Unhandled error
                }
            },
            errs => Task.FromResult(-1)
        ).ConfigureAwait(true);

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
            Console.WriteLine(resourceManager.GetString("ManagedWarmupText", CultureInfo.InvariantCulture));
            Console.ForegroundColor = ConsoleColor.Gray;

            FftManaged.WarmUp(Params.Log2FftSize, Params.FftRepeat);

            // Benchmark
            Console.ForegroundColor = ConsoleColor.Yellow;
            Console.WriteLine(resourceManager.GetString("ManagedText", CultureInfo.InvariantCulture));
            Console.ForegroundColor = ConsoleColor.Gray;

            managedElapsedMillisecond = FftManaged.Calculate(Params.Log2FftSize, Params.FftRepeat);
        }

        if (nativeBenchmark)
        {
            try
            {
                // Benchmark
                Console.ForegroundColor = ConsoleColor.Yellow;
                Console.WriteLine(resourceManager.GetString("NativeText", CultureInfo.InvariantCulture));
                Console.ForegroundColor = ConsoleColor.Gray;

                nativeElapsedMillisecond = FftNative.Calculate(Params.Log2FftSize, Params.FftRepeat);
            }
            catch (Exception e)
            {
                Console.WriteLine(resourceManager.GetString("CanotRunNative", CultureInfo.InvariantCulture)!, e.Message);
                Console.WriteLine(resourceManager.GetString("HaveYouCompiledNative", CultureInfo.InvariantCulture));
                return 1;
            }
        }

        if (mathNetBenchmark)
        {
            // Warmup
            Console.ForegroundColor = ConsoleColor.Green;
            Console.WriteLine(resourceManager.GetString("MathNetWarmupText", CultureInfo.InvariantCulture));
            Console.ForegroundColor = ConsoleColor.Gray;

            FftMathNet.WarmUp(Params.Log2FftSize, Params.FftRepeat);

            // Benchmark
            Console.ForegroundColor = ConsoleColor.Yellow;
            Console.WriteLine(resourceManager.GetString("MathNetText", CultureInfo.InvariantCulture));
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
