using System;
using System.Buffers;
using System.Diagnostics;
using System.Numerics;
using System.Runtime.CompilerServices;

namespace CSharpFftDemo;

internal static partial class FftManaged
{
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void InitializeTestData(Complex[] xy, int size)
    {
        int halfSize = size / 2;
        
        for (int i = 0; i < halfSize; i++)
        {
            xy[i] = Complex.One;
        }

        for (int i = halfSize; i < size; i++)
        {
            xy[i] = new Complex(-1.0, 0.0);
        }
    }

    public static double Calculate(int log2FftSize, int fftRepeat)
    {
        int size = 1 << log2FftSize;
        
        // Use ArrayPool to reduce GC pressure
        Complex[] xy = ArrayPool<Complex>.Shared.Rent(size);
        Complex[] xy_out = ArrayPool<Complex>.Shared.Rent(size);

        try
        {
            // Initialize test data once with optimized method
            InitializeTestData(xy, size);

            // FFT
            Stopwatch stopwatch = Stopwatch.StartNew();

            for (int i = 0; i < fftRepeat; i++)
            {
                Calculate(log2FftSize, xy.AsSpan(0, size), xy_out.AsSpan(0, size));
            }

            stopwatch.Stop();

            Console.WriteLine($"Total ({fftRepeat}): {stopwatch.ElapsedMilliseconds}");

            float tpp = stopwatch.ElapsedMilliseconds / (float)fftRepeat;

            Console.WriteLine($"{fftRepeat} piece(s) of {1 << log2FftSize} pt FFT;  {tpp} ms/piece\n");

            for (int i = 0; i < 6; i++)
            {
                Console.WriteLine($"{i} {xy_out[i]}");
            }

            return tpp;
        }
        finally
        {
            // Return arrays to pool
            ArrayPool<Complex>.Shared.Return(xy);
            ArrayPool<Complex>.Shared.Return(xy_out);
        }
    }

    public static void WarmUp(int log2FftSize, int fftRepeat)
    {
        int size = 1 << log2FftSize;
        
        // Use ArrayPool to reduce GC pressure
        Complex[] xy = ArrayPool<Complex>.Shared.Rent(size);
        Complex[] xy_out = ArrayPool<Complex>.Shared.Rent(size);

        try
        {
            // Initialize test data once with optimized method
            InitializeTestData(xy, size);

            // JIT warm up ... possible gives more speed
            for (int i = 0; i < fftRepeat; i++)
            {
                Calculate(log2FftSize, xy.AsSpan(0, size), xy_out.AsSpan(0, size));
            }
        }
        finally
        {
            // Return arrays to pool
            ArrayPool<Complex>.Shared.Return(xy);
            ArrayPool<Complex>.Shared.Return(xy_out);
        }
    }
}