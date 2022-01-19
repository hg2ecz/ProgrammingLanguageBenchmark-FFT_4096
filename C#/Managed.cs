using System;
using System.Diagnostics;
using System.Numerics;

namespace CSharpFftDemo;

internal static class FftManaged
{
    public static double Calculate(int log2FftSize, int fftRepeat)
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

        return tpp;
    }
}