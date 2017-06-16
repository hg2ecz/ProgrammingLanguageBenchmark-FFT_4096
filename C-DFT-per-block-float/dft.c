#include <math.h>
#include "dft.h"

static int dft_point = 0;
static float complex phasediffXY;

void dft_init(int point) {
    phasediffXY = cosf(2*M_PI/point) + I*sinf(2*M_PI/point);
    dft_point = point;
}

void dft(float complex *xy_out, const float complex *xy_in) {
    float complex phaseXY = 1.;

    for (int i = 0; i<dft_point; i++) {
	float complex vfoXY = 1.;
	xy_out[i] = 0.;
	for(int j=0; j<dft_point; j++) {
	    xy_out[i] += xy_in[j]*vfoXY;
	    vfoXY *= phaseXY;
	}
	phaseXY *= phasediffXY;
    }
}
