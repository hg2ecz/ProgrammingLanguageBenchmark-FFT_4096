using System;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace CSharpFftDemo
{
    public static class FftNative
    {
        [StructLayout(LayoutKind.Sequential)]
        public struct FloatComplex
        {
            public float Real;
            public float Imaginary;

            public FloatComplex(float real, float imaginary)
            {
                Real = real;
                Imaginary = imaginary;
            }

            public override string ToString()
            {
                return $"(Re: {Real}, Im: {Imaginary}";
            }
        }

        [DllImport("fft.so")]
        public static extern void fft(int log2point, [Out] FloatComplex[] xy_out, FloatComplex[] xy_in);

        public static void Native(int log2FftSize, int fftRepeat)
        {
            int i;
            int size = 1 << log2FftSize;
            FloatComplex[] xy = new FloatComplex[size];
            FloatComplex[] xy_out = new FloatComplex[xy.Length];

            for (i = 0; i < size / 2; i++)
                xy[i] = new FloatComplex(1.0f, 0.0f);

            for (i = size / 2; i < size; i++)
                xy[i] = new FloatComplex(-1.0f, 0.0f);

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
        }
    }
}