using System;
using System.Diagnostics;
using System.Numerics;
using System.Runtime.InteropServices;

namespace CSharpFftDemo
{
    public static class Benchmark
    {
        const int Log2FftSize = 12;
        const int FftRepeat = 1000;

        static void Main()
        {
            Console.WriteLine("---- MANAGED ----");
            Managed();

            try
            {
                Console.WriteLine("---- NATIVE ----");
                Native();
            }
            catch (Exception e)
            {
                Console.WriteLine($"Can not run native method: {e.Message}");
                Console.WriteLine($"Have you successfully compiled the project in C-fast-all-complex-float-types-opt-4x?");
            }
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct DoubleComplex
        {
            public float Real;
            public float Imaginary;

            public DoubleComplex(double real, double imaginary)
            {
                Real = (float)real;
                Imaginary = (float)imaginary;
            }
    
            public override string ToString()
            {
                return $"(R: {Real}, I: {Imaginary}";
            }
        }

        [DllImport("../C-fast-all-complex-float-types-opt-4x/fft.so")]
        public static extern void fft(int log2point, [Out] DoubleComplex[] xy_out, DoubleComplex[] xy_in);

        static void Native()
        {
            int i;
            int size = 1 << Log2FftSize;
            DoubleComplex[] xy = new DoubleComplex[size];
            DoubleComplex[] xy_out = new DoubleComplex[xy.Length];

            for (i = 0; i < size / 2; i++)
                xy[i] = new DoubleComplex(1.0, 0.0);

            for (i = size/2; i < size; i++)
                xy[i] = new DoubleComplex(-1.0, 0.0);

            var stopwatch = Stopwatch.StartNew();

            for (i = 0; i < FftRepeat; i++)
            {
                fft(Log2FftSize, xy_out, xy);
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

        static void Managed()
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
