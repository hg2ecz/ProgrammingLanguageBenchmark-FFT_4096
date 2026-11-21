using System;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace CSharpFftDemo;

internal static partial class FftNativeC
{
    [StructLayout(LayoutKind.Explicit)]
    internal struct DoubleComplex(double real, double imaginary)
    {
        [FieldOffset(0)]
        public double Real = real;

        [FieldOffset(8)]
        public double Imaginary = imaginary;

        public override readonly string ToString()
        {
            return $"(Re: {Real}, Im: {Imaginary})";
        }
    }

    [LibraryImport("./libfft.so", EntryPoint = "fft")]
    [DefaultDllImportSearchPaths(DllImportSearchPath.ApplicationDirectory)]
    internal static partial void Fft(int log2point, DoubleComplex[] xy_out, DoubleComplex[] xy_in);

    public static double Calculate(int log2FftSize, int fftRepeat)
    {
        int i;
        int size = 1 << log2FftSize;

        DoubleComplex[] xy = new DoubleComplex[size];
        DoubleComplex[] xy_out = new DoubleComplex[xy.Length];

        for (i = 0; i < size / 2; i++)
        {
            xy[i] = new DoubleComplex(1.0f, 0.0f);
        }

        for (i = size / 2; i < size; i++)
        {
            xy[i] = new DoubleComplex(-1.0f, 0.0f);
        }

        Stopwatch stopwatch = Stopwatch.StartNew();

        for (i = 0; i < fftRepeat; i++)
        {
            Fft(log2FftSize, xy_out, xy);
        }

        stopwatch.Stop();

        Console.WriteLine($"Total ({fftRepeat}): {stopwatch.Elapsed.TotalMilliseconds:F1}");

        double tpp = stopwatch.Elapsed.TotalMilliseconds / fftRepeat;

        Console.WriteLine($"{fftRepeat} piece(s) of {1 << log2FftSize} pt FFT;  {tpp} ms/piece\n");

        for (i = 0; i < 6; i++)
        {
            Console.WriteLine($"{i} {xy_out[i]}");
        }

        return tpp;
    }
}
