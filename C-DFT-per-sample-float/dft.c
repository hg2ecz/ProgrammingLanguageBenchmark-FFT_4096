#include <math.h>
#include "dft.h"

static int phasevec_exist = 0;
static double complex phasevec[33];

static float complex vfoXY[1<<LOG2FFTSIZE];


void dft_init() {
    if (!phasevec_exist) {
	for (int i=0; i<=32; i++) {
	    int point = 1<<i;
	    phasevec[i] = cos(2*M_PI/point) + I*sin(2*M_PI/point);
	}
	phasevec_exist = 1;
    }
    for (int i=0; i< 1<<LOG2FFTSIZE; i++) vfoXY[i]=1.;
}


void dft_sample(int point, float complex *xy_out, float complex xy_sample) {
    // correction
    float complex corr=0;
    double complex phasediffXY = phasevec[LOG2FFTSIZE];

    double complex phaseXY = 1.;
    for(int i=0; i<point; i++) {
	vfoXY[i] *= phaseXY;
	corr += xy_out[i]*conjf(vfoXY[i]);
	phaseXY *= phasediffXY;
    }
    xy_sample -= corr/point;

    // dft - sample
    for(int i=0; i<point; i++) {
	xy_out[i] += xy_sample*vfoXY[i];
    }
}
