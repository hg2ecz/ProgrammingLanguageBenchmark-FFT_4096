using System;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace FftBenchmark;

internal static class FftManaged
{
    private const int PhaseVecLength = 32;

    // phasevec[i] = e^(-2*pi*j / 2^(i+1)), the twiddle-factor step of stage i.
    private static readonly Complex[] phasevec = CreatePhaseVec();

    private static Complex[] CreatePhaseVec()
    {
        var result = new Complex[PhaseVecLength];

        for (int i = 0; i < PhaseVecLength; i++)
        {
            double phase = -2.0 * Math.PI / Math.Pow(2.0, i + 1);
            result[i] = new Complex(Math.Cos(phase), Math.Sin(phase));
        }

        return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static int ReverseBits(int value, int bitCount)
    {
        uint v = (uint)value;
        v = ((v & 0xAAAAAAAA) >> 1) | ((v & 0x55555555) << 1);
        v = ((v & 0xCCCCCCCC) >> 2) | ((v & 0x33333333) << 2);
        v = ((v & 0xF0F0F0F0) >> 4) | ((v & 0x0F0F0F0F) << 4);
        v = ((v & 0xFF00FF00) >> 8) | ((v & 0x00FF00FF) << 8);
        v = (v >> 16) | (v << 16);
        return (int)(v >> (32 - bitCount));
    }

    public static void Calculate(int log2FftSize, ReadOnlySpan<Complex> xyIn, Span<Complex> xyOut)
    {
        int n = 1 << log2FftSize;

        if (xyIn.Length < n || xyOut.Length < n)
        {
            throw new ArgumentException($"Buffers must hold at least {n} elements.");
        }

        ref Complex inRef = ref MemoryMarshal.GetReference(xyIn);
        ref Complex outRef = ref MemoryMarshal.GetReference(xyOut);

        // Bit-reversal permutation
        for (int i = 0; i < n; i++)
        {
            Unsafe.Add(ref outRef, ReverseBits(i, log2FftSize)) = Unsafe.Add(ref inRef, i);
        }

        // First stage (mmax = 1): the twiddle factor is always 1.
        int mmax = 1;
        int l2pt = 1;

        if (n > 1)
        {
            for (int i = 0; i < n; i += 2)
            {
                ref Complex upper = ref Unsafe.Add(ref outRef, i);
                ref Complex lower = ref Unsafe.Add(ref outRef, i + 1);

                Complex temp = lower;
                lower = upper - temp;
                upper += temp;
            }

            mmax = 2;
        }

        // Danielson-Lanczos section
        while (n > mmax)
        {
            int istep = mmax << 1;
            Complex wphase = phasevec[l2pt++];
            Complex w = Complex.One;

            for (int m = 0; m < mmax; m++)
            {
                double wReal = w.Real;
                double wImag = w.Imaginary;

                for (int i = m; i < n; i += istep)
                {
                    ref Complex upperRef = ref Unsafe.Add(ref outRef, i);
                    ref Complex lowerRef = ref Unsafe.Add(ref outRef, i + mmax);

                    // Complex multiplication expanded by hand: temp = w * lower
                    double lowerReal = lowerRef.Real;
                    double lowerImag = lowerRef.Imaginary;

                    double tempReal = (wReal * lowerReal) - (wImag * lowerImag);
                    double tempImag = (wReal * lowerImag) + (wImag * lowerReal);

                    double upperReal = upperRef.Real;
                    double upperImag = upperRef.Imaginary;

                    lowerRef = new Complex(upperReal - tempReal, upperImag - tempImag);
                    upperRef = new Complex(upperReal + tempReal, upperImag + tempImag);
                }

                w *= wphase; // rotate
            }

            mmax = istep;
        }
    }
}
