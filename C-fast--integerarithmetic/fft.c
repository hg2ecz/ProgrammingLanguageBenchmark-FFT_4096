#include <math.h>
//#include <malloc.h>
#include "fft.h"

// Internal variables
static int phasevec_exist = 0;
static int phasevec[32][2];

// Public function
void fft(int log2point, struct _sample *restrict xy_out, const struct _sample *restrict xy_in) {
    int i;
    if (!phasevec_exist) {
	for (i=0; i<32; i++) {
	    int point = 2<<i;
	    phasevec[i][0] = (1<<INTMUL)*cos(-2*M_PI/point);
	    phasevec[i][1] = (1<<INTMUL)*sin(-2*M_PI/point);
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
	xy_out->fract[brev][0] = xy_in->fract[i][0];
	xy_out->fract[brev][1] = xy_in->fract[i][1];
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
//	struct _sample wphase_XY = cos(theta) + sin(theta)*I;
	int wphase_XY[2] = { phasevec[l2pt][0], phasevec[l2pt][1] };
	l2pt++;

	int w_XY[2] = { 1.*(1<<INTMUL), 0 };
	for (int m=0; m < mmax; m++) {
	    for (int i=m; i < n; i += istep) {
		int tempXY[2];
		tempXY[0] = ((long long)w_XY[0] * xy_out->fract[i+mmax][0] - (long long)w_XY[1] * xy_out->fract[i+mmax][1]) >> INTMUL;
		tempXY[1] = ((long long)w_XY[0] * xy_out->fract[i+mmax][1] + (long long)w_XY[1] * xy_out->fract[i+mmax][0]) >> INTMUL;
		xy_out->fract[i+mmax][0]  = xy_out->fract[i][0] - tempXY[0];
		xy_out->fract[i+mmax][1]  = xy_out->fract[i][1] - tempXY[1];
		xy_out->fract[i     ][0] += tempXY[0];
		xy_out->fract[i     ][1] += tempXY[1];
	    }
	    int w_tmp = ((long long)w_XY[0] * wphase_XY[0] - (long long)w_XY[1] * wphase_XY[1]) >> INTMUL;
	    w_XY[1] = ((long long)w_XY[0] * wphase_XY[1] + (long long)w_XY[1] * wphase_XY[0]) >> INTMUL;
	    w_XY[0] = w_tmp;
	}
	mmax=istep;
    }
}
