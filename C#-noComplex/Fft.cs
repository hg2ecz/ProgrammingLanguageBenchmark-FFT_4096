using System;
using System.Diagnostics;

namespace CSharpFftDemo
{
    public class Fft {
        // Internal variables
        private double[,] phasevec = new double[32,2];

        public Fft()
        {
            for (int i = 0; i < 32; i++)
            {
                int point = 2 << i;
                phasevec[i,0] = Math.Cos(-2 * Math.PI / point);
                phasevec[i,1] = Math.Sin(-2 * Math.PI / point);
            }
        }

        // Public function
        public void Calc(int Log2FftSize, double[,] xy_in, double[,] xy_out)
        {

            for (int i = 0; i < (1 << Log2FftSize); i++)
            {
                long brev = i;

                brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1);
                brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2);
                brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4);
                brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8);
                brev = (brev >> 16) | (brev << 16);

                brev >>= 32 - Log2FftSize;
                xy_out[brev,0] = xy_in[i,0];
                xy_out[brev,1] = xy_in[i,1];
                // Complex is a struct in .NET, so passed by value.
                // new Complex(xy_in[i].Real, xy_in[i].Imaginary);
            }

            int n = 1 << Log2FftSize;
            int l2pt = 0;
            int mmax = 1;

            while (n > mmax)
            {
                int istep = mmax << 1;

                double wphase_X = phasevec[l2pt,0];
                double wphase_Y = phasevec[l2pt,1];
                l2pt++;

                double w_X = 1.0;
                double w_Y = 0.0;

                for (int m = 0; m < mmax; m++)
                {
                    for (int i = m; i < n; i += istep)
                    {
                        var tempX = w_X * xy_out[i + mmax,0] - w_Y * xy_out[i + mmax,1];
                        var tempY = w_X * xy_out[i + mmax,1] + w_Y * xy_out[i + mmax,0];

                        xy_out[i + mmax,0] = xy_out[i,0] - tempX;
                        xy_out[i + mmax,1] = xy_out[i,1] - tempY;
                        xy_out[i,0] += tempX;
                        xy_out[i,1] += tempY;
                    }

                    var w_Xtemp = w_X * wphase_X - w_Y * wphase_Y;
                    w_Y         = w_X * wphase_Y + w_Y * wphase_X;
                    w_X = w_Xtemp;
                }
                mmax = istep;
            }
        }
    }
}
