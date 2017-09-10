#include <math.h>
#include "fft.h"

// Internal variables
static int phasevec_exist = 0;
static COMPLEX_VFO_TYPE phasevec[32];

// Public function
void fft(int log2point, COMPLEX_TYPE *restrict xy_out, const COMPLEX_TYPE *restrict xy_in) {
    int i;
    if (!phasevec_exist) {
	for (i=0; i<32; i++) {
	    int point = 2<<i;
	    phasevec[i] = cos(-2*M_PI/point) + sin(-2*M_PI/point)*I;
	}
	phasevec_exist = 1;
    }
    for (i=0; i < (1<<log2point); i++) {
	unsigned int brev = i;
	brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1);
	brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2);
	brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4);
	brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8);
	brev = (brev >> 16) | (brev << 16);

	brev >>= 32-log2point;
	xy_out[brev] = xy_in[i];
    }

    // here begins the Danielson-Lanczos section
    int n = 1<<log2point;
    int l2pt=0;
    int mmax=1;

    for (int i=0; i < n; i += 2) {
	COMPLEX_TYPE tempXY = xy_out[i+mmax]; // w_XY = 1
	xy_out[i+mmax]  = xy_out[i] - tempXY;
	xy_out[i  ] += tempXY;
    }
    l2pt++;
    mmax<<=1;

#ifdef MOD_SPEED
    while (l2pt < log2point) {
	int istep = 2<<l2pt;
#else
    while (n>mmax) {
	int istep = mmax<<1;
#endif
//	double theta = -2*M_PI/istep;
//	COMPLEX_TYPE wphase_XY = cos(theta) + sin(theta)*I;
	COMPLEX_VFO_TYPE wphase_XY = phasevec[l2pt++];

	COMPLEX_VFO_TYPE w_XY = 1.0 + 0.0*I;
	COMPLEX_VFO_TYPE w_XY2;
	for (int m=0; m < mmax; m+=2) { // optimization: tempXY and tempXY2
	    w_XY2 = w_XY * wphase_XY; // rotate
	    for (int i=m; i < n; i += istep) {
		COMPLEX_TYPE tempXY = (COMPLEX_TYPE)w_XY *xy_out[i+mmax];
		xy_out[i+mmax]  = xy_out[i] - tempXY;
		xy_out[i     ] += tempXY;

		COMPLEX_TYPE tempXY2 = (COMPLEX_TYPE)w_XY2 *xy_out[i+1+mmax];
		xy_out[i+1+mmax]  = xy_out[i+1] - tempXY2;
		xy_out[i+1     ] += tempXY2;
	    }
	    w_XY = w_XY2 * wphase_XY; // rotate
	}
	mmax=istep;
    }
}
