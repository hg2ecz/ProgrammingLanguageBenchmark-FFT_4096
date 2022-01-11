using System;
using System.Diagnostics;

namespace CSharpFftDemo
{
    public static class Benchmark
    {
        private const int Log2FftSize = 12;
        private const int FftRepeat = 20000;

        private static void Main()
        {
            int i;
            const int size = 1 << Log2FftSize;
            double[,] xy = new double[size,2];
            double[,] xy_out = new double[xy.Length,2];

            for (i = 0; i < size / 2; i++) {
                xy[i,0] = 1.0;
                xy[i,1] = 0.0;
            }

            for (i = size/2; i < size; i++) {
                xy[i,0] = -1.0;
                xy[i,1] =  0.0;
            }

            Fft fft = new Fft();

            // JIT warm up ... possible give more speed
            for (i = 0; i < FftRepeat; i++)
            {
                fft.Calc(Log2FftSize, xy, xy_out);
            }
            // FFT
            var stopwatch = Stopwatch.StartNew();

            for (i = 0; i < FftRepeat; i++)
            {
                fft.Calc(Log2FftSize, xy, xy_out);
            }

            stopwatch.Stop();

            Console.WriteLine($"Total ({FftRepeat}): {stopwatch.ElapsedMilliseconds}");

            var tpp = stopwatch.ElapsedMilliseconds / (float)FftRepeat;

            Console.WriteLine($"{FftRepeat} piece(s) of {1 << Log2FftSize} pt FFT;  {tpp} ms/piece\n");

            for (i = 0; i < 6; i++)
            {
                Console.WriteLine("{0}\t{1}\t{2}", i, xy_out[i,0], xy_out[i,1]);
            }
        }
    }
}
