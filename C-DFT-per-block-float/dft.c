#include <math.h>
#include "dft.h"

static int phasevec_exist = 0;
static float complex phasevec[33];

void dft_init() {
    for (int i=0; i<=32; i++) {
	int point = 1<<i;
	phasevec[i] = cos(2*M_PI/point) + I*sin(2*M_PI/point);
    }
    phasevec_exist = 1;
}


void dft(int log2point, float complex *xy_out, const float complex *xy_in) {
    if (!phasevec_exist) dft_init();

    const int point = 1<<log2point;
    float complex phaseXY = 1.;
    const float complex phasediffXY = phasevec[log2point];

    for (int i = 0; i<point; i++) {
	float complex vfoXY = 1.;
	xy_out[i] = 0.;
	for(int j=0; j<point; j++) {
	    xy_out[i] += xy_in[j]*vfoXY;
	    vfoXY *= phaseXY;
	}
	phaseXY *= phasediffXY;
    }
}
