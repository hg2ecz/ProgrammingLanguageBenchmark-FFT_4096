using System;
using System.Diagnostics;
using System.Numerics;

namespace CSharpFftDemo
{
    public class Benchmark
    {
        const int Log2FftSize = 12;
        const int FftRepeat = 1000;

        // Internal variables
        static Complex[] phasevec = null;


        private static Complex s_one = Complex.One;

        // Public function
        public unsafe static Complex[] FFT(Complex[] xy_in, Complex[] xy_out)
        {
            PhasevecInit();


            for (int i = 0; i < (1 << Log2FftSize); i++)
            {
                long brev = i;

                brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1);
                brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2);
                brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4);
                brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8);
                brev = (brev >> 16) | (brev << 16);

                brev >>= 32 - Log2FftSize;
                xy_out[brev] = xy_in[i]; 
                // Complex is a struct in .NET, so passed by value.
                // new Complex(xy_in[i].Real, xy_in[i].Imaginary);
            }

            int n = 1 << Log2FftSize;
            int l2pt = 0;
            int mmax = 1;

            while (n > mmax)
            {
                int istep = mmax << 1;

                var wphase_XY = phasevec[l2pt++];

                // Same.
                var w_XY = s_one;


                for (int m = 0; m < mmax; m++)
                {
                    for (int i = m; i < n; i += istep)
                    {
                        var tempXY = w_XY * xy_out[i + mmax];

                        xy_out[i + mmax] = xy_out[i] - tempXY;
                        xy_out[i] = xy_out[i] + tempXY;
                    }

                    w_XY = w_XY * wphase_XY;
                }
                mmax = istep;
            }

            return xy_out;
        }

        // Should not try to init every time.
        private static void PhasevecInit()
        {
            if (phasevec != null)
                return;

            phasevec = new Complex[32];
            

            for (int i = 0; i < 32; i++)
            {
                int point = 2 << i;
                phasevec[i] = new Complex(Math.Cos(-2 * Math.PI / point), Math.Sin(-2 * Math.PI / point));
            }
        }



        static void Main(string[] args)
        {
            PhasevecInit();

            int i;
            int size = 1 << Log2FftSize;
            Complex[] xy = new Complex[size];
            Complex[] xy_out = new Complex[xy.Length];


            for (i = 0; i < size / 2; i++)
                xy[i] = new Complex(1.0, 0.0);

            for (i = size/2; i < size; i++)
                xy[i] = new Complex(-1.0, 0.0);


            // JIT warm up ... possible give more speed
            for (i = 0; i < FftRepeat; i++)
            {
                FFT(xy, xy_out);
            }
            // FFT
            var stopwatch = Stopwatch.StartNew();


            for (i = 0; i < FftRepeat; i++)
            {
                FFT(xy, xy_out);
            }

            stopwatch.Stop();

            Console.WriteLine($"Total ({FftRepeat}): {stopwatch.ElapsedMilliseconds}");

            var tpp = stopwatch.ElapsedMilliseconds / (float)FftRepeat;

            Console.WriteLine($"{FftRepeat} piece(s) of {1 << Log2FftSize} pt FFT;  {tpp} ms/piece\n");

            var result = FFT(xy, xy_out);

            for (i = 0; i < 6; i++)
            {
                Console.WriteLine("{0}\t{1}", i, result[i]);
            }
        }
    }
}
