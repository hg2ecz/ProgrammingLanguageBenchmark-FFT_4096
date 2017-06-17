#include <math.h>
#include "dft.h"

static int dft_point = 0;
static float dft_revpt;

static double complex dft_phasediffXY;
static float complex dft_vfoXY[1<<LOG2FFTSIZE];


void dft_init(int point) {
    dft_point = point;
    dft_revpt = 1./point;
    //phasediffXY = cos(-2*M_PI/point) + I*sin(-2*M_PI/point);
    dft_phasediffXY = cexp(-2j*M_PI/point);

    for (int i=0; i<point; i++) dft_vfoXY[i]=1.;
}

static float complex dft_corr=0;
void dft_sample(float complex *xy_out, float complex xy_sample) {
    xy_sample -= dft_corr*dft_revpt;		// remove oldest value from DFT

    dft_corr = 0;
    double complex phaseXY = 1.;

    for(int i=0; i<dft_point; i++) {
	float complex tmp = dft_vfoXY[i];
	xy_out[i] += xy_sample*tmp;

	tmp *= phaseXY;				// oscillator new state
	phaseXY *= dft_phasediffXY;		// oscillator new speed
	dft_corr += xy_out[i]*conjf(tmp);	// generate new prev. correction
	dft_vfoXY[i]=tmp;
    }
}
