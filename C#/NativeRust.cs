using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace FftBenchmark;

/// <summary>
/// The Rust implementation (rust/src/lib.rs) loaded from libfftr.so.
/// The native object holds the precomputed twiddle factors.
/// </summary>
internal sealed unsafe partial class NativeRust : SafeHandle
{
    public NativeRust()
        : base(IntPtr.Zero, ownsHandle: true)
    {
        SetHandle(New());

        if (IsInvalid)
        {
            throw new InvalidOperationException("The Rust library could not create an FFT object.");
        }
    }

    public override bool IsInvalid => handle == IntPtr.Zero;

    public void Calculate(int log2FftSize, ReadOnlySpan<Complex> xyIn, Span<Complex> xyOut)
    {
        int n = 1 << log2FftSize;

        if (xyIn.Length < n || xyOut.Length < n)
        {
            throw new ArgumentException($"Buffers must hold at least {n} elements.");
        }

        fixed (Complex* pIn = xyIn)
        fixed (Complex* pOut = xyOut)
        {
            Fft(handle, pOut, pIn, n);
        }
    }

    protected override bool ReleaseHandle()
    {
        Free(handle);
        return true;
    }

    [LibraryImport("libfftr", EntryPoint = "new")]
    [DefaultDllImportSearchPaths(DllImportSearchPath.AssemblyDirectory)]
    private static partial IntPtr New();

    [LibraryImport("libfftr", EntryPoint = "fft")]
    [DefaultDllImportSearchPaths(DllImportSearchPath.AssemblyDirectory)]
    private static partial void Fft(IntPtr self, Complex* xyOut, Complex* xyIn, int length);

    [LibraryImport("libfftr", EntryPoint = "free")]
    [DefaultDllImportSearchPaths(DllImportSearchPath.AssemblyDirectory)]
    private static partial void Free(IntPtr self);
}
