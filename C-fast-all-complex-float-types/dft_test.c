#include <stdio.h>
#include <math.h>
#include <complex.h>
#include "fft.h"
#include "dft_test.h"

void dft(COMPLEX_TYPE *out, const COMPLEX_TYPE *in, unsigned int log2point) {
    const int pointnum = 1<<log2point;
    for (int harmonic = 0; harmonic < pointnum; harmonic++) {
	complex double sumreg = 0;
	for (int si=0; si<pointnum; si++) { // sample index
	    double osc_phase = -2*M_PI*harmonic * si/pointnum; // 0..harmonic round
	    complex double oscillator = cos(osc_phase) + I*sin(osc_phase);
	    sumreg += oscillator * in[si];
	}
	out[harmonic] = sumreg;
    }
}

void dft_test(const COMPLEX_TYPE *fftout, const COMPLEX_TYPE *in, int log2fft, double relerror) {
    COMPLEX_TYPE dftout[1<<log2fft];
    dft(dftout, in, log2fft);
    double maxval = 1;
    for (int i=0; i<(1<<log2fft); i++) {
	if (maxval < cabs(dftout[i])) maxval = cabs(dftout[i]);
    }
    double err = maxval * relerror;
    fprintf(stderr, "\n Max absolute value: %f, relative error limit: %f\n", maxval, relerror);
    for (int i=0; i < (1<<log2fft); i++) {
	if (fabs(creal(fftout[i])-creal(dftout[i])) > err || fabs(cimag(fftout[i])-cimag(dftout[i])) > err) {
	    fprintf(stderr, " Inaccuracy error: fft%d: %d., re reok: %.5f %.5f :: im, imok: %.5fj %.5fj\n",
                log2fft, i, creal(fftout[i]), creal(dftout[i]), cimag(fftout[i]), cimag(dftout[i]));
	}
    }
}
