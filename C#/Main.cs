using System;

namespace FftBenchmark;

internal sealed class Options
{
    public bool DotnetBenchmark { get; set; }
    public bool Managed { get; set; }
    public bool NativeC { get; set; }
    public bool NativeRust { get; set; }
    public int FftRepeat { get; set; } = 20000;
    public int Log2FftSize { get; set; } = 12;

    /// <summary>Parses the command line; returns null if the program should exit with <paramref name="exitCode"/>.</summary>
    public static Options? Parse(string[] args, out int exitCode)
    {
        var options = new Options();
        exitCode = 0;

        for (int i = 0; i < args.Length; i++)
        {
            switch (args[i])
            {
                case "-d" or "--dotnet-benchmark":
                    options.DotnetBenchmark = true;
                    break;
                case "-m" or "--managed":
                    options.Managed = true;
                    break;
                case "-n" or "--native":
                    options.NativeC = true;
                    break;
                case "-u" or "--rust":
                    options.NativeRust = true;
                    break;
                case "-r" or "--repeat" when TryReadInt(args, ref i, 1, int.MaxValue, out int repeat):
                    options.FftRepeat = repeat;
                    break;
                case "-s" or "--size" when TryReadInt(args, ref i, 1, 30, out int log2Size):
                    options.Log2FftSize = log2Size;
                    break;
                case "-h" or "--help":
                    PrintHelp();
                    return null;
                default:
                    Console.Error.WriteLine($"Invalid or incomplete option: {args[i]}");
                    PrintHelp();
                    exitCode = 2;
                    return null;
            }
        }

        // Nothing selected: run the managed benchmark only.
        if (!options.DotnetBenchmark && !options.Managed && !options.NativeC && !options.NativeRust)
        {
            options.Managed = true;
        }

        return options;
    }

    private static bool TryReadInt(string[] args, ref int i, int min, int max, out int value)
    {
        if (i + 1 < args.Length && int.TryParse(args[i + 1], out value) && value >= min && value <= max)
        {
            i++;
            return true;
        }

        value = 0;
        return false;
    }

    private static void PrintHelp()
    {
        Console.WriteLine("""
            FFT Benchmark Tool
            Usage: fft-benchmark [options]

            Options:
              -m, --managed              Run the managed C# implementation (default if nothing is selected)
              -n, --native               Run the native C implementation (libfft.so)
              -u, --rust                 Run the native Rust implementation (libfftr.so)
              -d, --dotnet-benchmark     Run all implementations with BenchmarkDotNet
              -r, --repeat <N>           Number of iterations (default: 20000)
              -s, --size <N>             Log2 of the FFT size, 1..30 (default: 12, i.e. 4096 points)
              -h, --help                 Show this help message
            """);
    }
}

internal static class Program
{
    public static int Main(string[] args)
    {
        Options? options = Options.Parse(args, out int exitCode);

        if (options is null)
        {
            return exitCode;
        }

        try
        {
            Run(options);
            return 0;
        }
        catch (DllNotFoundException e)
        {
            Console.Error.WriteLine($"Can not load native library: {e.Message}");
            Console.Error.WriteLine("Have you built it with 'make'?");
            return 1;
        }
    }

    private static void Run(Options options)
    {
        Console.WriteLine($"Log2FftSize: {options.Log2FftSize}, Repeat: {options.FftRepeat}");

        int log2 = options.Log2FftSize;
        int repeat = options.FftRepeat;
        double? managedMs = null;

        if (options.DotnetBenchmark)
        {
            DotnetBenchmark.Run(log2);
        }

        if (options.Managed)
        {
            managedMs = Measurement.Run("MANAGED", log2, repeat, FftManaged.Calculate, warmUp: true);
        }

        if (options.NativeC)
        {
            double nativeMs = Measurement.Run("NATIVE", log2, repeat, NativeC.Calculate);
            PrintRatio("Native", managedMs, nativeMs);
        }

        if (options.NativeRust)
        {
            using var rust = new NativeRust();
            double rustMs = Measurement.Run("RUST", log2, repeat, rust.Calculate);
            PrintRatio("Rust", managedMs, rustMs);
        }
    }

    private static void PrintRatio(string name, double? managedMs, double otherMs)
    {
        if (managedMs is not double managed)
        {
            return;
        }

        Console.ForegroundColor = ConsoleColor.Green;
        Console.WriteLine($"{name} Ratio: {managed / otherMs:0.####}");
        Console.WriteLine($"{name} Diff%: {(managed / otherMs) - 1:0.##%}");
        Console.ResetColor();
    }
}
