using System;
using System.Diagnostics;
using System.Globalization;
using System.Numerics;
using System.Reflection;
using System.Resources;
using MathNet.Numerics;
using MathNet.Numerics.IntegralTransforms;

using static CSharpFftDemo.GlobalResourceManager;

namespace CSharpFftDemo;

internal static class FftMathNet
{
    public static void SetupMathNet()
    {
        Console.ForegroundColor = ConsoleColor.Green;
        Console.WriteLine(GetStringResource("MathNetSetupText"));
        Console.ForegroundColor = ConsoleColor.Gray;

        if (Control.TryUseNativeCUDA())
        {
            Console.WriteLine(GetStringResource("MathNetCudaText"));
            return;
        }
        else if (Control.TryUseNativeMKL())
        {
            Console.WriteLine(GetStringResource("MathNetMklText"));
            return;
        }
        else if (Control.TryUseNativeOpenBLAS())
        {
            Console.WriteLine(GetStringResource("MathNetOpenBlasText"));
            return;
        }
        else if (Control.TryUseNative())
        {
            Console.WriteLine(GetStringResource("MathNetNativeText"));
            return;
        }
        else
        {
            Console.WriteLine(GetStringResource("MathNetManagedText"));
            Console.WriteLine(GetStringResource("MathNetMiltithreadingText"));
            Control.UseMultiThreading();
        }
    }

    public static double Calculate(int log2FftSize, int fftRepeat)
    {
        int i;
        int size = 1 << log2FftSize;
        Complex[] xy = new Complex[size];

        for (i = 0; i < size / 2; i++)
        {
            xy[i] = new Complex(1.0, 0.0);
        }

        for (i = size / 2; i < size; i++)
        {
            xy[i] = new Complex(-1.0, 0.0);
        }

        // FFT
        var stopwatch = Stopwatch.StartNew();

        for (i = 0; i < fftRepeat; i++)
        {
            Fourier.Forward(xy);

            if (i == 0)
            {
                for (i = 0; i < 6; i++)
                {
                    Console.WriteLine(GetStringResource("ZeroTabOne")!, i, xy[i]);
                }
            }
        }

        stopwatch.Stop();

        Console.WriteLine($"Total ({fftRepeat}): {stopwatch.ElapsedMilliseconds}");

        var tpp = stopwatch.ElapsedMilliseconds / (float)fftRepeat;

        Console.WriteLine($"{fftRepeat} piece(s) of {1 << log2FftSize} pt FFT;  {tpp} ms/piece\n");

        return tpp;
    }

    public static void WarmUp(int log2FftSize, int fftRepeat)
    {
        int i;
        int size = 1 << log2FftSize;
        Complex[] xy = new Complex[size];

        for (i = 0; i < size / 2; i++)
        {
            xy[i] = new Complex(1.0, 0.0);
        }

        for (i = size / 2; i < size; i++)
        {
            xy[i] = new Complex(-1.0, 0.0);
        }

        // JIT warm up ... possible give more speed
        for (i = 0; i < fftRepeat; i++)
        {
            Fourier.Forward(xy);
        }
    }
}