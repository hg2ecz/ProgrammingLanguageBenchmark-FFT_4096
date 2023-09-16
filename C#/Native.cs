using System;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace CSharpFftDemo;

internal static partial class FftNative
{
    [StructLayout(LayoutKind.Sequential)]
    public struct DoubleComplex
    {
        public double Real;
        public double Imaginary;

        public DoubleComplex(double real, double imaginary)
        {
            Real = real;
            Imaginary = imaginary;
        }

        public override readonly string ToString()
        {
            return $"(Re: {Real}, Im: {Imaginary})";
        }
    }

    [LibraryImport("libfft.so", EntryPoint = "fft")]
    internal static partial void fft(int log2point, DoubleComplex[] xy_out, DoubleComplex[] xy_in);

    public static double Calculate(int log2FftSize, int fftRepeat)
    {
        int i;
        int size = 1 << log2FftSize;

        var xy = new DoubleComplex[size];
        var xy_out = new DoubleComplex[xy.Length];

        for (i = 0; i < size / 2; i++)
            xy[i] = new DoubleComplex(1.0f, 0.0f);

        for (i = size / 2; i < size; i++)
            xy[i] = new DoubleComplex(-1.0f, 0.0f);

        var stopwatch = Stopwatch.StartNew();

        for (i = 0; i < fftRepeat; i++)
        {
            fft(log2FftSize, xy_out, xy);
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
