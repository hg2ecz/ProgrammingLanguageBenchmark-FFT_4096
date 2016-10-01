using System;
using System.Diagnostics;
using System.Numerics;

namespace CSharpFftDemo
{
    public class Benchmark
    {
        static int LOG2FFTSIZE = 16;
        static int FFT_REPEAT = 10;

        // Internal variables
        static int phasevec_exist = 0;
        static Complex[] phasevec = new Complex[32];

        // Public function
        public static Complex[] FFT(int log2point, Complex[] xy_in)
        {
            Complex[] xy_out = new Complex[xy_in.Length];

            if (phasevec_exist == 0)
            {
                for (int i = 0; i < 32; i++)
                {
                    int point = 2 << i;
                    phasevec[i] = new Complex(Math.Cos(-2 * Math.PI / point), Math.Sin(-2 * Math.PI / point));
                }
                phasevec_exist = 1;
            }

            for (int i = 0; i < (1 << log2point); i++)
            {
                long brev = i;
                brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1);
                brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2);
                brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4);
                brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8);
                brev = (brev >> 16) | (brev << 16);

                brev >>= 32 - log2point;
                xy_out[brev] = new Complex(xy_in[i].Real, xy_in[i].Imaginary);
            }

            int n = 1 << log2point;
            int l2pt = 0;
            int mmax = 1;

            while (n > mmax)
            {
                int istep = mmax << 1;

                Complex wphase_XY = phasevec[l2pt++];
                Complex w_XY = new Complex(1.0, 0.0);

                for (int m = 0; m < mmax; m++)
                {
                    for (int i = m; i < n; i += istep)
                    {
                        Complex tempXY = w_XY * xy_out[i + mmax];

                        xy_out[i + mmax] = xy_out[i] - tempXY;
                        xy_out[i] = xy_out[i] + tempXY;
                    }

                    w_XY = w_XY * wphase_XY;
                }
                mmax = istep;
            }

            return xy_out;
        }

        static void Main(string[] args)
        {
            int i;
            int SIZE = 1 << LOG2FFTSIZE;
            Complex[] xy = new Complex[SIZE];

            for (i = 0; i < SIZE / 2; i++)
            {
                xy[i] = new Complex(1.0, 0.0);
            }

            for (i = SIZE/2; i < SIZE; i++)
            {
                xy[i] = new Complex(-1.0, 0.0);
            }

            // FFT
            var stopwatch = Stopwatch.StartNew();

            for (i = 0; i < FFT_REPEAT; i++)
            {
                FFT(LOG2FFTSIZE, xy);
            }

            Console.WriteLine("{0} piece(s) of {1} pt FFT;  {2} ms/piece\n",
                FFT_REPEAT,
                1 << LOG2FFTSIZE,
                stopwatch.ElapsedMilliseconds / FFT_REPEAT);

            var result = FFT(LOG2FFTSIZE, xy);

            for (i = 0; i < 6; i++)
            {
                Console.WriteLine("{0}\t{1}\n", i, result[i]);
            }
        }
    }
}
