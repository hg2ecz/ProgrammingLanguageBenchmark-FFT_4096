#include <stdio.h>
#include <math.h>
#include <complex.h>
#include "fft.h"
#include "dft_test.h"

void dft(struct _complexblock *out, const struct  _complexblock *in, unsigned int log2point) {
    const int pointnum = 1<<log2point;
    for (int harmonic = 0; harmonic < pointnum; harmonic++) {
	complex double sumreg = 0;
	for (int si=0; si<pointnum; si++) { // sample index
	    double osc_phase = -2*M_PI*harmonic * si/pointnum; // 0..harmonic round
	    complex double oscillator = cos(osc_phase) + I*sin(osc_phase);
	    sumreg += oscillator * (in->re[si] + I*in->im[si]);
	}
	out->re[harmonic] = creal(sumreg);
	out->im[harmonic] = cimag(sumreg);
    }
}

void dft_test(const struct _complexblock *fftout, const struct _complexblock *in, int log2fft, double relerror) {
    struct _complexblock dftout;
    dft(&dftout, in, log2fft);
    double maxval = 1;
    for (int i=0; i<(1<<log2fft); i++) {
	if (maxval < cabs(dftout.re[i]+I*dftout.im[i])) maxval = cabs(dftout.re[i]+I*dftout.im[i]);
    }
    double err = maxval * relerror;
    fprintf(stderr, "\n Max absolute value: %f, relative error limit: %f\n", maxval, relerror);
    for (int i=0; i < (1<<log2fft); i++) {
	if (fabs(fftout->re[i]-dftout.re[i]) > err || fabs(fftout->im[i]-dftout.im[i]) > err) {
	    fprintf(stderr, " Inaccuracy error: fft%d: %d., re reok: %.5f %.5f :: im, imok: %.5fj %.5fj, relerr: %.9f\n",
                log2fft, i, fftout->re[i], dftout.re[i], fftout->im[i], dftout.im[i],
		(cabs(fftout->re[i]+I*fftout->im[i])-cabs(dftout.re[i]+I*dftout.im[i]))/maxval);
	}
    }
}
