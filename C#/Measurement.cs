using System;
using System.Diagnostics;
using System.Numerics;

namespace FftBenchmark;

internal delegate void FftFunction(int log2FftSize, ReadOnlySpan<Complex> xyIn, Span<Complex> xyOut);

/// <summary>
/// Runs an FFT implementation repeatedly on the same square-wave input and reports
/// the time per transform in the format expected by ../avgcalc.py.
/// </summary>
internal static class Measurement
{
    private const int PrintedOutputCount = 6;

    /// <summary>Square wave: +1 for the first half, -1 for the second half.</summary>
    public static Complex[] CreateTestInput(int size)
    {
        var xy = new Complex[size];
        xy.AsSpan(0, size / 2).Fill(Complex.One);
        xy.AsSpan(size / 2).Fill(new Complex(-1.0, 0.0)); // not -Complex.One, which has a -0 imaginary part
        return xy;
    }

    /// <returns>Milliseconds per transform.</returns>
    public static double Run(string title, int log2FftSize, int fftRepeat, FftFunction fft, bool warmUp = false)
    {
        int size = 1 << log2FftSize;
        Complex[] xyIn = CreateTestInput(size);
        var xyOut = new Complex[size];

        if (warmUp)
        {
            WriteTitle($"{title} (warmup)", ConsoleColor.Green);

            for (int i = 0; i < fftRepeat; i++)
            {
                fft(log2FftSize, xyIn, xyOut);
            }
        }

        WriteTitle(title, ConsoleColor.Yellow);

        var stopwatch = Stopwatch.StartNew();

        for (int i = 0; i < fftRepeat; i++)
        {
            fft(log2FftSize, xyIn, xyOut);
        }

        stopwatch.Stop();

        double totalMs = stopwatch.Elapsed.TotalMilliseconds;
        double msPerPiece = totalMs / fftRepeat;

        Console.WriteLine($"Total ({fftRepeat}): {totalMs:F1}");
        Console.WriteLine($"{fftRepeat} piece(s) of {size} pt FFT;  {msPerPiece:F5} ms/piece");
        Console.WriteLine();

        for (int i = 0; i < Math.Min(PrintedOutputCount, size); i++)
        {
            Console.WriteLine($"{i}\t{xyOut[i]}");
        }

        return msPerPiece;
    }

    public static void WriteTitle(string title, ConsoleColor color)
    {
        Console.ForegroundColor = color;
        Console.WriteLine($"---- {title} ----");
        Console.ResetColor();
    }
}
