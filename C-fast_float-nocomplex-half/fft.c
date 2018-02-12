#include <math.h>
//#include <malloc.h>
#include "fft.h"

// Internal variables
static int phasevec_exist = 0;
static struct _sample phasevec[32];

// Public function
void fft(int log2point, struct _sample *restrict xy_out, const struct _sample *restrict xy_in) {
    if (!phasevec_exist) {
	for (int i=0; i<32; i++) {
	    int point = 2<<i;
	    phasevec[i].i = cos(-2*M_PI/point);
	    phasevec[i].q = sin(-2*M_PI/point);
	}
	phasevec_exist = 1;
    }
    for (int i=0; i < (1<<log2point); i++) {
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
#ifdef MOD_SPEED
    while (l2pt < log2point) {
	int istep = 2<<l2pt;
#else
    while (n>mmax) {
	int istep = mmax<<1;
#endif
//	int theta = -2*M_PI/istep;
//	double complex wphase_XY = cos(theta) + sin(theta)*I;
	struct _sample wphase_XY = phasevec[l2pt];
	l2pt++;

	struct _sample w_XY = { 1., 0. };
	for (int m=0; m < mmax; m++) {
	    for (int i=m; i < n; i += istep) {
		struct _sample tempXY;
		tempXY.i = w_XY.i * xy_out[i+mmax].i - w_XY.q * xy_out[i+mmax].q;
		tempXY.q = w_XY.i * xy_out[i+mmax].q + w_XY.q * xy_out[i+mmax].i;
		xy_out[i+mmax].i  = xy_out[i].i - tempXY.i;
		xy_out[i+mmax].q  = xy_out[i].q - tempXY.q;
		xy_out[i     ].i += tempXY.i;
		xy_out[i     ].q += tempXY.q;
	    }
	    __fp16 w_tmp = w_XY.i * wphase_XY.i - w_XY.q * wphase_XY.q;
	    w_XY.q      = w_XY.i * wphase_XY.q + w_XY.q * wphase_XY.i;
	    w_XY.i      = w_tmp;
	}
	mmax=istep;
    }
}
