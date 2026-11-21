using System;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace CSharpFftDemo;

internal static partial class FftNativeRust
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

    internal sealed partial class FftHandle : SafeHandle
    {
        public FftHandle() : base(0, true)
        {
            SetHandle(NewHandle());
        }

        protected override bool ReleaseHandle()
        {
            FreeHandle(handle);
            return true;
        }

        public override bool IsInvalid
        {
            get
            {
                return handle == IntPtr.Zero;
            }
        }

        public void Fft(DoubleComplex[] xy_out, DoubleComplex[] xy_in, int size)
        {
            Fft(handle, xy_out, xy_in, size);
        }

        [LibraryImport("./libfftr.so", EntryPoint = "new")]
        [DefaultDllImportSearchPaths(DllImportSearchPath.ApplicationDirectory)]
        internal static partial IntPtr NewHandle();

        [LibraryImport("./libfftr.so", EntryPoint = "fft")]
        [DefaultDllImportSearchPaths(DllImportSearchPath.ApplicationDirectory)]
        internal static partial void Fft(IntPtr self_ptr, DoubleComplex[] xy_out, DoubleComplex[] xy_in, int length);

        [LibraryImport("./libfftr.so", EntryPoint = "free")]
        [DefaultDllImportSearchPaths(DllImportSearchPath.ApplicationDirectory)]
        internal static partial void FreeHandle(IntPtr self_ptr);
    }

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

        // Create the handle outside the measured section; a missing library
        // (DllNotFoundException) propagates to the caller.
        using FftHandle fftHandle = new FftHandle();

        Stopwatch stopwatch = Stopwatch.StartNew();

        for (i = 0; i < fftRepeat; i++)
        {
            fftHandle.Fft(xy_out, xy, size);
        }

        stopwatch.Stop();

        Console.WriteLine($"Total ({fftRepeat}): {stopwatch.Elapsed.TotalMilliseconds:F1}");

        double tpp = stopwatch.Elapsed.TotalMilliseconds / fftRepeat;

        Console.WriteLine($"{fftRepeat} piece(s) of {1 << log2FftSize} pt FFT;  {tpp} ms/piece\n");

        for (i = 0; i < 6; i++)
        {
            Console.WriteLine($"{i}\t{xy_out[i]}");
        }

        return tpp;
    }
}