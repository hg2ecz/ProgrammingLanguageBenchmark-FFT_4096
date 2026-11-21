using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace FftBenchmark;

/// <summary>
/// The C implementation (other_tests/C-tests/C-fast_double) loaded from libfft.so.
/// </summary>
internal static unsafe partial class NativeC
{
    // void fft(int log2point, double complex *xy_out, const double complex *xy_in);
    // System.Numerics.Complex has the same layout as C's double complex.
    [LibraryImport("libfft", EntryPoint = "fft")]
    [DefaultDllImportSearchPaths(DllImportSearchPath.AssemblyDirectory)]
    private static partial void Fft(int log2point, Complex* xyOut, Complex* xyIn);

    public static void Calculate(int log2FftSize, ReadOnlySpan<Complex> xyIn, Span<Complex> xyOut)
    {
        int n = 1 << log2FftSize;

        if (xyIn.Length < n || xyOut.Length < n)
        {
            throw new ArgumentException($"Buffers must hold at least {n} elements.");
        }

        fixed (Complex* pIn = xyIn)
        fixed (Complex* pOut = xyOut)
        {
            Fft(log2FftSize, pOut, pIn);
        }
    }
}
