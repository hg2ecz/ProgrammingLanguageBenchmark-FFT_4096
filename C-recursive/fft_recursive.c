#include <math.h>
#include "fft_recursive.h"

void fft_recursive(double complex *restrict buf, double complex *restrict out, int n, int step) {
    if (step < n) {
	fft_recursive(out, buf, n, step * 2);
	fft_recursive(out + step, buf + step, n, step * 2);
 
	double complex phase = 1.;
	double complex phasediff = cexp(-I * M_PI * (2*step)/ n); // jó

	for (int i = 0; i < n; i += 2 * step) {
	    double complex t = phase * out[i + step];
	    buf[i / 2]     = out[i] + t;
	    buf[(i + n)/2] = out[i] - t;
	    phase *= phasediff;
	}
    }
}
