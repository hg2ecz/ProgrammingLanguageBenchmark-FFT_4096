#include <math.h>
#include "dft.h"

static int dft_point = 0;
static double complex phasediffXY;

void dft_init(int point) {
    phasediffXY = cos(2*M_PI/point) + I*sin(2*M_PI/point);
    dft_point = point;
}

void dft(double complex *xy_out, const double complex *xy_in) {
    double complex phaseXY = 1.;

    for (int i = 0; i<dft_point; i++) {
	double complex vfoXY = 1.;
	xy_out[i] = 0.;
	for(int j=0; j<dft_point; j++) {
	    xy_out[i] += xy_in[j]*vfoXY;
	    vfoXY *= phaseXY;
	}
	phaseXY *= phasediffXY;
    }
}
