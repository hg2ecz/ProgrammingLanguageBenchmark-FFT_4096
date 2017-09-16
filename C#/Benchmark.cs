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
            Console.ForegroundColor = ConsoleColor.Green;
            Console.WriteLine("---- MANAGED ----");
            Console.ForegroundColor = ConsoleColor.Gray;

            Managed();

            try
            {
                Console.ForegroundColor = ConsoleColor.Green;
                Console.WriteLine("---- NATIVE ----");
                Console.ForegroundColor = ConsoleColor.Gray;

                Native();
            }
            catch (Exception e)
            {
                Console.WriteLine($"Can not run native method: {e.Message}");
                Console.WriteLine($"Have you successfully compiled the project in C-fast-all-complex-float-types-opt-4x?");
            }
        }

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

        [DllImport("../C-fast-all-complex-float-types-opt-4x/fft.so")]
        public static extern void fft(int log2point, [Out] FloatComplex[] xy_out, FloatComplex[] xy_in);

        static void Native()
        {
            int i;
            int size = 1 << Log2FftSize;
            FloatComplex[] xy = new FloatComplex[size];
            FloatComplex[] xy_out = new FloatComplex[xy.Length];

            for (i = 0; i < size / 2; i++)
                xy[i] = new FloatComplex(1.0f, 0.0f);

            for (i = size/2; i < size; i++)
                xy[i] = new FloatComplex(-1.0f, 0.0f);

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
