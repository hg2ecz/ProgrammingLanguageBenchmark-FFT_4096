using System;
using System.Diagnostics;
using System.Numerics;

namespace CSharpFftDemo
{
    public static class Benchmark
    {
        const int Log2FftSize = 12;
        const int FftRepeat = 1000;

        static void Main()
        {
            int i;
            int size = 1 << Log2FftSize;
            Complex[] xy = new Complex[size];
            Complex[] xy_out = new Complex[xy.Length];

            for (i = 0; i < size / 2; i++)
                xy[i] = new Complex(1.0, 0.0);

            for (i = size/2; i < size; i++)
                xy[i] = new Complex(-1.0, 0.0);

            Fft fft = new Fft();

            // JIT warm up ... possible give more speed
            for (i = 0; i < FftRepeat; i++)
            {
                fft.Calculate(Log2FftSize, xy, xy_out);
            }

            // FFT
            var stopwatch = Stopwatch.StartNew();

            for (i = 0; i < FftRepeat; i++)
            {
                fft.Calculate(Log2FftSize, xy, xy_out);
            }

            stopwatch.Stop();

            Console.WriteLine($"Total ({FftRepeat}): {stopwatch.ElapsedMilliseconds}");

            var tpp = stopwatch.ElapsedMilliseconds / (float)FftRepeat;

            Console.WriteLine($"{FftRepeat} piece(s) of {1 << Log2FftSize} pt FFT;  {tpp} ms/piece\n");

            for (i = 0; i < 6; i++)
            {
                Console.WriteLine("{0}\t{1}", i, xy_out[i]);
            }
        }
    }
}
