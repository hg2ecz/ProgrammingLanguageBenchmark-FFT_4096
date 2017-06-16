#include <math.h>
#include "dft.h"

static int dft_point = 0;
static float dft_revpt;

static double complex phasediffXY;
static float complex vfoXY[1<<LOG2FFTSIZE];


void dft_init(int point) {
    dft_point = point;
    dft_revpt = 1./point;
    phasediffXY = cos(2*M_PI/point) + I*sin(2*M_PI/point);

    for (int i=0; i< 1<<LOG2FFTSIZE; i++) vfoXY[i]=1.;
}


void dft_sample(float complex *xy_out, float complex xy_sample) {
    // correction
    float complex corr=0;
    double complex phaseXY = 1.;

    for(int i=0; i<dft_point; i++) {
	vfoXY[i] *= phaseXY;
	phaseXY *= phasediffXY;
	corr += xy_out[i]*conjf(vfoXY[i]);
    }
    xy_sample -= corr*dft_revpt;

    // dft - sample
    for(int i=0; i<dft_point; i++) {
	xy_out[i] += xy_sample*(float complex)vfoXY[i];
    }
}
