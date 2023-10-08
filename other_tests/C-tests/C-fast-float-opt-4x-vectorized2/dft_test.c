#include <stdio.h>
#include <math.h>
#include <complex.h>
#include "fft.h"

void dft(complex float *cplxout, const complex float *cplxin, unsigned int log2point) {
    const int pointnum = 1<<log2point;
    for (int harmonic = 0; harmonic < pointnum; harmonic++) {
	complex double sumreg = 0;
	for (int si=0; si<pointnum; si++) { // sample index
	    double osc_phase = -2*M_PI*harmonic * si/pointnum; // 0..harmonic round
	    complex double oscillator = cos(osc_phase) + I*sin(osc_phase);
	    sumreg += oscillator * cplxin[si];
	}
	cplxout[harmonic] = sumreg;
    }
}

void dft_test(const complex float *cplxout, const complex float *cplxin, int log2fft, double relerror) {
    complex float dftout[1<<log2fft];
    dft(dftout, cplxin, log2fft);
    double maxval = 1;
    for (int i=0; i<(1<<log2fft); i++) {
	if (maxval < cabs(dftout[i])) maxval = cabs(dftout[i]);
    }
    double err = maxval * relerror;
    fprintf(stderr, "\n Max absolute value: %f, relative error limit: %f\n", maxval, relerror);
    for (int i=0; i < (1<<log2fft); i++) {
	if (cabs(cplxout[i]-dftout[i]) > err) {
	    fprintf(stderr, " Inaccuracy error: fft%d: %d., re reok: %.5f %.5f :: im, imok: %.5fj %.5fj, relerr: %.9f\n",
                log2fft, i, creal(cplxout[i]), creal(dftout[i]), cimag(cplxout[i]), cimag(dftout[i]),
		(cabs(cplxout[i]-dftout[i]))/maxval);
	}
    }
}
