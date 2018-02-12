#include <cmath>
#include "fft.h"

// Public function
fft::fft(void) {
    for (int i=0; i<32; i++) {
	int point = 2<<i;
	phasevec[i] = cos(-2*M_PI/point) + sin(-2*M_PI/point)*1i;
    }
}

void fft::calc(int log2point, std::complex<double> *xy_out, const std::complex<double> *xy_in) {
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
//	double theta = -2*M_PI/istep;
//	double complex wphase_XY = cos(theta) + sin(theta)*I;
	std::complex<double> wphase_XY = phasevec[l2pt++];

	std::complex<double> w_XY = 1.0 + 0.0i;
	for (int m=0; m < mmax; m++) {
	    for (int i=m; i < n; i += istep) {
		std::complex<double> tempXY = w_XY *xy_out[i+mmax];
		xy_out[i+mmax]  = xy_out[i] - tempXY;
		xy_out[i     ] += tempXY;
	    }
	    w_XY *= wphase_XY; // rotate
	}
	mmax=istep;
    }
}
