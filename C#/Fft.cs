using System;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace CSharpFftDemo;

internal static partial class FftManaged
{
    private static readonly Complex[] phasevec = [
            new Complex(-1, -1.22464679914735E-16),
            new Complex(6.12323399573677E-17, -1),
            new Complex(0.707106781186548, -0.707106781186548),
            new Complex(0.923879532511287, -0.38268343236509),
            new Complex(0.98078528040323, -0.195090322016128),
            new Complex(0.995184726672197, -0.0980171403295606),
            new Complex(0.998795456205172, -0.049067674327418),
            new Complex(0.999698818696204, -0.0245412285229123),
            new Complex(0.999924701839145, -0.0122715382857199),
            new Complex(0.999981175282601, -0.00613588464915448),
            new Complex(0.999995293809576, -0.00306795676296598),
            new Complex(0.999998823451702, -0.00153398018628477),
            new Complex(0.999999705862882, -0.000766990318742704),
            new Complex(0.999999926465718, -0.000383495187571396),
            new Complex(0.999999981616429, -0.000191747597310703),
            new Complex(0.999999995404107, -9.58737990959773E-05),
            new Complex(0.999999998851027, -4.79368996030669E-05),
            new Complex(0.999999999712757, -2.39684498084182E-05),
            new Complex(0.999999999928189, -1.19842249050697E-05),
            new Complex(0.999999999982047, -5.99211245264243E-06),
            new Complex(0.999999999995512, -2.99605622633466E-06),
            new Complex(0.999999999998878, -1.49802811316901E-06),
            new Complex(0.999999999999719, -7.49014056584716E-07),
            new Complex(0.99999999999993, -3.74507028292384E-07),
            new Complex(0.999999999999982, -1.87253514146195E-07),
            new Complex(0.999999999999996, -9.36267570730981E-08),
            new Complex(0.999999999999999, -4.68133785365491E-08),
            new Complex(1, -2.34066892682746E-08),
            new Complex(1, -1.17033446341373E-08),
            new Complex(1, -5.85167231706864E-09),
            new Complex(1, 2.92583615853432E-09),
            new Complex(1, 0)
        ];

    // Optimized bit reversal helper
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static int ReverseBits(int value, int bitCount)
    {
        // More efficient bit reversal using divide-and-conquer
        uint v = (uint)value;
        v = ((v & 0xAAAAAAAA) >> 1) | ((v & 0x55555555) << 1);
        v = ((v & 0xCCCCCCCC) >> 2) | ((v & 0x33333333) << 2);
        v = ((v & 0xF0F0F0F0) >> 4) | ((v & 0x0F0F0F0F) << 4);
        v = ((v & 0xFF00FF00) >> 8) | ((v & 0x00FF00FF) << 8);
        v = (v >> 16) | (v << 16);
        return (int)(v >> (32 - bitCount));
    }

    // AVX2/FMA optimized butterfly for x86-64-v3
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static unsafe void ButterflyAvx2Fma(ref Complex upper, ref Complex lower, double wReal, double wImag)
    {
        if (Fma.IsSupported && Avx2.IsSupported)
        {
            // Load complex numbers as [real, imag]
            fixed (Complex* pUpper = &upper, pLower = &lower)
            {
                Vector128<double> lowerVec = Sse2.LoadVector128((double*)pLower);  // [lower.real, lower.imag]
                Vector128<double> upperVec = Sse2.LoadVector128((double*)pUpper);  // [upper.real, upper.imag]
                
                // Broadcast w components
                Vector128<double> wRealVec = Vector128.Create(wReal);    // [w.real, w.real]
                Vector128<double> wImagVec = Vector128.Create(wImag);    // [w.imag, w.imag]
                
                // Complex multiplication using FMA: temp = w * lower
                // temp.real = w.real * lower.real - w.imag * lower.imag
                // temp.imag = w.real * lower.imag + w.imag * lower.real
                
                // Multiply w.real * lower
                Vector128<double> temp1 = Fma.Multiply(wRealVec, lowerVec);  // [w.real*lower.real, w.real*lower.imag]
                
                // Swap lower components and multiply by w.imag with sign correction
                Vector128<double> lowerSwapped = Sse2.Shuffle(lowerVec, lowerVec, 0b01);  // [lower.imag, lower.real]
                Vector128<double> signMask = Vector128.Create(-1.0, 1.0);  // Fixed: negate real, keep imag positive
                Vector128<double> wImagSigned = Sse2.Multiply(wImagVec, signMask);  // [-w.imag, w.imag]
                
                // FMA: temp = temp1 + wImagSigned * lowerSwapped = [w.real*lower.real - w.imag*lower.imag, w.real*lower.imag + w.imag*lower.real]
                Vector128<double> tempVec = Fma.MultiplyAdd(wImagSigned, lowerSwapped, temp1);
                
                // Butterfly: lower = upper - temp, upper = upper + temp
                Vector128<double> lowerResult = Sse2.Subtract(upperVec, tempVec);
                Vector128<double> upperResult = Sse2.Add(upperVec, tempVec);
                
                // Store results
                Sse2.Store((double*)pLower, lowerResult);
                Sse2.Store((double*)pUpper, upperResult);
            }
        }
        else
        {
            // Fallback to scalar
            double lowerReal = lower.Real;
            double lowerImag = lower.Imaginary;
            
            double tempReal = wReal * lowerReal - wImag * lowerImag;
            double tempImag = wReal * lowerImag + wImag * lowerReal;
            
            double upperReal = upper.Real;
            double upperImag = upper.Imaginary;
            
            lower = new Complex(upperReal - tempReal, upperImag - tempImag);
            upper = new Complex(upperReal + tempReal, upperImag + tempImag);
        }
    }

    // Process 2 butterflies in parallel with AVX2 (4 complex numbers)
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static unsafe void Butterfly2xAvx2Fma(
        ref Complex upper1, ref Complex lower1, 
        ref Complex upper2, ref Complex lower2,
        double wReal, double wImag)
    {
        if (Fma.IsSupported && Avx2.IsSupported)
        {
            fixed (Complex* pU1 = &upper1, pL1 = &lower1, pU2 = &upper2, pL2 = &lower2)
            {
                // Load 4 complex numbers as 256-bit vectors
                Vector256<double> lower = Avx.LoadVector256((double*)pL1);  // [l1.r, l1.i, l2.r, l2.i]
                Vector256<double> upper = Avx.LoadVector256((double*)pU1);  // [u1.r, u1.i, u2.r, u2.i]
                
                // Broadcast w components across all lanes
                Vector256<double> wRealVec = Vector256.Create(wReal);
                Vector256<double> wImagVec = Vector256.Create(wImag);
                
                // temp = w * lower using FMA
                Vector256<double> temp1 = Fma.Multiply(wRealVec, lower);
                
                // Permute for complex multiplication: [imag, real, imag, real]
                Vector256<double> lowerSwapped = Avx2.Permute4x64(lower, 0b10_11_00_01);
                Vector256<double> signMask = Vector256.Create(-1.0, 1.0, -1.0, 1.0);  // Fixed sign pattern
                Vector256<double> wImagSigned = Avx.Multiply(wImagVec, signMask);
                
                Vector256<double> tempVec = Fma.MultiplyAdd(wImagSigned, lowerSwapped, temp1);
                
                // Butterfly operations
                Vector256<double> lowerResult = Avx.Subtract(upper, tempVec);
                Vector256<double> upperResult = Avx.Add(upper, tempVec);
                
                // Store results
                Avx.Store((double*)pL1, lowerResult);
                Avx.Store((double*)pU1, upperResult);
            }
        }
        else
        {
            // Fallback to scalar
            ButterflyAvx2Fma(ref upper1, ref lower1, wReal, wImag);
            ButterflyAvx2Fma(ref upper2, ref lower2, wReal, wImag);
        }
    }

    // Public function
    public static unsafe void Calculate(int Log2FftSize, Span<Complex> xyIn, Span<Complex> xyOut)
    {
        int n = 1 << Log2FftSize;
        // Use refs to avoid repeated Span indexing/bounds checks
        ref Complex inRef = ref MemoryMarshal.GetReference(xyIn);
        ref Complex outRef = ref MemoryMarshal.GetReference(xyOut);

        // Optimized bit reversal with helper method
        for (int i = 0; i < n; i++)
        {
            int brev = ReverseBits(i, Log2FftSize);
            Unsafe.Add(ref outRef, brev) = Unsafe.Add(ref inRef, i);
        }

        int l2pt = 0;
        int mmax = 1;

        // Special case: mmax = 1 (first stage) - fully unrolled
        if (n > 1)
        {
            Complex wphase_XY = phasevec[l2pt++];
            
            // First stage is always with w = 1, so simplify
            for (int i = 0; i < n; i += 2)
            {
                ref Complex upper = ref Unsafe.Add(ref outRef, i);
                ref Complex lower = ref Unsafe.Add(ref outRef, i + 1);
                
                Complex temp = lower;
                lower = upper - temp;
                upper = upper + temp;
            }
            
            mmax = 2;
        }

        while (n > mmax)
        {
            int istep = mmax << 1;
            Complex wphase_XY = phasevec[l2pt++];
            Complex w_XY = Complex.One;

            for (int m = 0; m < mmax; m++)
            {
                // Cache w_XY components for better register allocation
                double wReal = w_XY.Real;
                double wImag = w_XY.Imaginary;
                
                // Process all butterflies for this m value
                for (int i = m; i < n; i += istep)
                {
                    ref Complex upperRef = ref Unsafe.Add(ref outRef, i);
                    ref Complex lowerRef = ref Unsafe.Add(ref outRef, i + mmax);
                    
                    // Manually expanded complex multiplication for best scalar performance
                    double lowerReal = lowerRef.Real;
                    double lowerImag = lowerRef.Imaginary;
                    
                    double tempReal = wReal * lowerReal - wImag * lowerImag;
                    double tempImag = wReal * lowerImag + wImag * lowerReal;
                    
                    double upperReal = upperRef.Real;
                    double upperImag = upperRef.Imaginary;
                    
                    lowerRef = new Complex(upperReal - tempReal, upperImag - tempImag);
                    upperRef = new Complex(upperReal + tempReal, upperImag + tempImag);
                }

                // Update w_XY exactly once per m (preserve original semantics)
                w_XY *= wphase_XY;
            }

            mmax = istep;
        }
    }
}
