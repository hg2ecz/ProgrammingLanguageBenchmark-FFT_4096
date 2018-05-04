#include <math.h>
#include "fft.h"

#if defined (__i386) || defined (__x86_64)
# include <immintrin.h>
#elif defined(__arm__) || defined (__aarch64__)
# include <arm_neon.h>
#endif

#if defined(__ARM_NEON)
typedef float32x4_t VECTORTYPE; // 4 pcs parallel
#elif defined(__SSE__)
typedef __m128 VECTORTYPE; // 4 pcs parallel
#endif


// Internal variables
#include "fft_const.h"

// Public function
void fft_vec(int log2point, FLOAT_TYPE *out_re, FLOAT_TYPE *out_im, const FLOAT_TYPE *in_re, const FLOAT_TYPE *in_im) {
    for (int i=0; i < (1<<log2point); i+=4) {
	unsigned int brev = i;
	brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1);
	brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2);
	brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4);
	brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8);
	brev = (brev >> 16) | (brev << 16);

	brev >>= 32-log2point;                          // 00 --> 00
	out_re[i] = in_re[brev];
	out_im[i] = in_im[brev];

	unsigned int brev1 = brev + (2<<(log2point-2)); // 01 --> 10
	out_re[i+1] = in_re[brev1];
	out_im[i+1] = in_im[brev1];

	unsigned int brev2 = brev + (1<<(log2point-2)); // 10 --> 01
	out_re[i+2] = in_re[brev2];
	out_im[i+2] = in_im[brev2];

	unsigned int brev3 = brev + (3<<(log2point-2)); // 11 --> 11
	out_re[i+3] = in_re[brev3];
	out_im[i+3] = in_im[brev3];
    }

    // here begins the Danielson-Lanczos section
    int n = 1<<log2point;
    int l2pt=0;
    int mmax=1;


    l2pt++;
    for (int i=0; i < n; i += 2) {
	FLOAT_TYPE tempX = out_re[i+mmax];
	FLOAT_TYPE tempY = out_im[i+mmax];
	out_re[i+mmax]  = out_re[i] - tempX;
	out_im[i+mmax]  = out_im[i] - tempY;
	out_re[i     ] += tempX;
	out_im[i     ] += tempY;
    }
    mmax<<=1;

    FLOAT_VFO_TYPE w_X2 = phasevec[l2pt][0];
    FLOAT_VFO_TYPE w_Y2 = phasevec[l2pt][1]; l2pt++;
    for (int i=0; i < n; i += 4) {
	FLOAT_TYPE tempX = out_re[i+mmax];
	FLOAT_TYPE tempY = out_im[i+mmax];
	out_re[i+mmax]  = out_re[i] - tempX;
	out_im[i+mmax]  = out_im[i] - tempY;
	out_re[i     ] += tempX;
	out_im[i     ] += tempY;

	FLOAT_TYPE tempX2 = (FLOAT_TYPE)w_X2 * out_re[i+1+mmax] - (FLOAT_TYPE)w_Y2 * out_im[i+1+mmax];
	FLOAT_TYPE tempY2 = (FLOAT_TYPE)w_X2 * out_im[i+1+mmax] + (FLOAT_TYPE)w_Y2 * out_re[i+1+mmax];
	out_re[i+1+mmax]  = out_re[i+1] - tempX2;
	out_im[i+1+mmax]  = out_im[i+1] - tempY2;
	out_re[i+1     ] += tempX2;
	out_im[i+1     ] += tempY2;
    }
    mmax<<=1;

#if defined (__i386) || defined (__x86_64)
    if (__builtin_cpu_supports ("avx")) {     // AVX: __v8sf
	typedef __v8sf AVX_TYPE;
	FLOAT_VFO_TYPE wphase_X = phasevec[l2pt][0];
	FLOAT_VFO_TYPE wphase_Y = phasevec[l2pt][1];

	VECTORTYPE wphase_Xvec, wphase_Yvec;
	wphase_Xvec[0] = wphase_Xvec[1] = wphase_Xvec[2] = wphase_Xvec[3]= phasevec[l2pt-2][0];
	wphase_Yvec[0] = wphase_Yvec[1] = wphase_Yvec[2] = wphase_Yvec[3]= phasevec[l2pt-2][1];
	l2pt++;

	VECTORTYPE w_Xvec, w_Yvec;
	w_Xvec[0] = 1.;
	w_Yvec[0] = 0.;

	w_Xvec[1] = w_Xvec[0] * wphase_X; // - w_Yvec[0] * wphase_Y;
	w_Yvec[1] = w_Xvec[0] * wphase_Y; // + w_Yvec[0] * wphase_X;

	w_Xvec[2] = w_Xvec[1] * wphase_X - w_Yvec[1] * wphase_Y;
	w_Yvec[2] = w_Xvec[1] * wphase_Y + w_Yvec[1] * wphase_X;

	w_Xvec[3] = w_Xvec[2] * wphase_X - w_Yvec[2] * wphase_Y;
	w_Yvec[3] = w_Xvec[2] * wphase_Y + w_Yvec[2] * wphase_X;
	for (int i=0; i < n; i += 8) {
	    VECTORTYPE *reg1_reptr = (VECTORTYPE *)&out_re[i+mmax]; // 4 lanes reg
	    VECTORTYPE *reg1_imptr = (VECTORTYPE *)&out_im[i+mmax]; // 4 lanes reg
	    VECTORTYPE reg1_re = *reg1_reptr;
	    VECTORTYPE reg1_im = *reg1_imptr;

	    VECTORTYPE temp_re = w_Xvec * reg1_re - w_Yvec * reg1_im; // 4 lanes mul
	    VECTORTYPE temp_im = w_Xvec * reg1_im + w_Yvec * reg1_re; // 4 lanes mul

	    VECTORTYPE *reg2_reptr = (VECTORTYPE *)&out_re[i]; // 4 lanes reg
	    VECTORTYPE *reg2_imptr = (VECTORTYPE *)&out_im[i]; // 4 lanes reg
	    VECTORTYPE reg2_re = *reg2_reptr;
	    VECTORTYPE reg2_im = *reg2_imptr;

	    *reg1_reptr = reg2_re - temp_re; // 4 lanes sub&store
	    *reg1_imptr = reg2_im - temp_im; // 4 lanes sub&store 
	    *reg2_reptr = reg2_re + temp_re; // 4 lanes add&store
	    *reg2_imptr = reg2_im + temp_im; // 4 lanes add&store
	}

	mmax<<=1;
	while (n>mmax) {
	    int istep = mmax<<1;
	    FLOAT_VFO_TYPE wphase_X = phasevec[l2pt][0];
	    FLOAT_VFO_TYPE wphase_Y = phasevec[l2pt][1];

	    AVX_TYPE wphase_Xvec, wphase_Yvec;
	    wphase_Xvec[0] = wphase_Xvec[1] = wphase_Xvec[2] = wphase_Xvec[3] =\
	    wphase_Xvec[4] = wphase_Xvec[5] = wphase_Xvec[6] = wphase_Xvec[7] = phasevec[l2pt-3][0];
	    wphase_Yvec[0] = wphase_Yvec[1] = wphase_Yvec[2] = wphase_Yvec[3] =\
	    wphase_Yvec[4] = wphase_Yvec[5] = wphase_Yvec[6] = wphase_Yvec[7] = phasevec[l2pt-3][1];
	    l2pt++;

	    AVX_TYPE w_Xvec, w_Yvec;
	    w_Xvec[0] = 1.;
	    w_Yvec[0] = 0.;

	    w_Xvec[1] = w_Xvec[0] * wphase_X; // - w_Yvec[0] * wphase_Y;
	    w_Yvec[1] = w_Xvec[0] * wphase_Y; // + w_Yvec[0] * wphase_X;

	    w_Xvec[2] = w_Xvec[1] * wphase_X - w_Yvec[1] * wphase_Y;
	    w_Yvec[2] = w_Xvec[1] * wphase_Y + w_Yvec[1] * wphase_X;

	    w_Xvec[3] = w_Xvec[2] * wphase_X - w_Yvec[2] * wphase_Y;
	    w_Yvec[3] = w_Xvec[2] * wphase_Y + w_Yvec[2] * wphase_X;

	    w_Xvec[4] = w_Xvec[3] * wphase_X - w_Yvec[3] * wphase_Y;
	    w_Yvec[4] = w_Xvec[3] * wphase_Y + w_Yvec[3] * wphase_X;

	    w_Xvec[5] = w_Xvec[4] * wphase_X - w_Yvec[4] * wphase_Y;
	    w_Yvec[5] = w_Xvec[4] * wphase_Y + w_Yvec[4] * wphase_X;

	    w_Xvec[6] = w_Xvec[5] * wphase_X - w_Yvec[5] * wphase_Y;
	    w_Yvec[6] = w_Xvec[5] * wphase_Y + w_Yvec[5] * wphase_X;

	    w_Xvec[7] = w_Xvec[6] * wphase_X - w_Yvec[6] * wphase_Y;
	    w_Yvec[7] = w_Xvec[6] * wphase_Y + w_Yvec[6] * wphase_X;

	    for (int m=0; m < mmax; m+=8) { // optimization: tempXY and tempXY2
		for (int i=m; i < n; i += istep) {
		    AVX_TYPE *reg1_reptr = (AVX_TYPE *)&out_re[i+mmax]; // 8 lanes reg
		    AVX_TYPE *reg1_imptr = (AVX_TYPE *)&out_im[i+mmax]; // 8 lanes reg
		    AVX_TYPE reg1_re = *reg1_reptr;
		    AVX_TYPE reg1_im = *reg1_imptr;

		    AVX_TYPE temp_re = w_Xvec * reg1_re - w_Yvec * reg1_im; // 8 lanes mul
		    AVX_TYPE temp_im = w_Xvec * reg1_im + w_Yvec * reg1_re; // 8 lanes mul

		    AVX_TYPE *reg2_reptr = (AVX_TYPE *)&out_re[i]; // 8 lanes reg
		    AVX_TYPE *reg2_imptr = (AVX_TYPE *)&out_im[i]; // 8 lanes reg
		    AVX_TYPE reg2_re = *reg2_reptr;
		    AVX_TYPE reg2_im = *reg2_imptr;

		    *reg1_reptr = reg2_re - temp_re; // 8 lanes sub&store
		    *reg1_imptr = reg2_im - temp_im; // 8 lanes sub&store 
		    *reg2_reptr = reg2_re + temp_re; // 8 lanes add&store
		    *reg2_imptr = reg2_im + temp_im; // 8 lanes add&store
		}
		AVX_TYPE w_Xtmp;
		w_Xtmp = w_Xvec * wphase_Xvec - w_Yvec * wphase_Yvec; // 4 lanes rotate
		w_Yvec = w_Xvec * wphase_Yvec + w_Yvec * wphase_Xvec; // 4 lanes rotate
		w_Xvec = w_Xtmp;
	    }
	    mmax=istep;
	}
    } else // x86: end AVX;    SSE, NEON
#endif
    {   // SSE, NEON
	while (n>mmax) {
	    int istep = mmax<<1;
	    FLOAT_VFO_TYPE wphase_X = phasevec[l2pt][0];
	    FLOAT_VFO_TYPE wphase_Y = phasevec[l2pt][1];

	    VECTORTYPE wphase_Xvec, wphase_Yvec;
	    wphase_Xvec[0] = wphase_Xvec[1] = wphase_Xvec[2] = wphase_Xvec[3]= phasevec[l2pt-2][0];
	    wphase_Yvec[0] = wphase_Yvec[1] = wphase_Yvec[2] = wphase_Yvec[3]= phasevec[l2pt-2][1];
	    l2pt++;

	    VECTORTYPE w_Xvec, w_Yvec;
	    w_Xvec[0] = 1.;
	    w_Yvec[0] = 0.;

	    w_Xvec[1] = w_Xvec[0] * wphase_X; // - w_Yvec[0] * wphase_Y;
	    w_Yvec[1] = w_Xvec[0] * wphase_Y; // + w_Yvec[0] * wphase_X;

	    w_Xvec[2] = w_Xvec[1] * wphase_X - w_Yvec[1] * wphase_Y;
	    w_Yvec[2] = w_Xvec[1] * wphase_Y + w_Yvec[1] * wphase_X;

	    w_Xvec[3] = w_Xvec[2] * wphase_X - w_Yvec[2] * wphase_Y;
	    w_Yvec[3] = w_Xvec[2] * wphase_Y + w_Yvec[2] * wphase_X;

	    for (int m=0; m < mmax; m+=4) { // optimization: tempXY and tempXY2
		for (int i=m; i < n; i += istep) {
		    VECTORTYPE *reg1_reptr = (VECTORTYPE *)&out_re[i+mmax]; // 4 lanes reg
		    VECTORTYPE *reg1_imptr = (VECTORTYPE *)&out_im[i+mmax]; // 4 lanes reg
		    VECTORTYPE reg1_re = *reg1_reptr;
		    VECTORTYPE reg1_im = *reg1_imptr;

		    VECTORTYPE temp_re = w_Xvec * reg1_re - w_Yvec * reg1_im; // 4 lanes mul
		    VECTORTYPE temp_im = w_Xvec * reg1_im + w_Yvec * reg1_re; // 4 lanes mul

		    VECTORTYPE *reg2_reptr = (VECTORTYPE *)&out_re[i]; // 4 lanes reg
		    VECTORTYPE *reg2_imptr = (VECTORTYPE *)&out_im[i]; // 4 lanes reg
		    VECTORTYPE reg2_re = *reg2_reptr;
		    VECTORTYPE reg2_im = *reg2_imptr;

		    *reg1_reptr = reg2_re - temp_re; // 4 lanes sub&store
		    *reg1_imptr = reg2_im - temp_im; // 4 lanes sub&store 
		    *reg2_reptr = reg2_re + temp_re; // 4 lanes add&store
		    *reg2_imptr = reg2_im + temp_im; // 4 lanes add&store
		}
		VECTORTYPE w_Xtmp;
		w_Xtmp = w_Xvec * wphase_Xvec - w_Yvec * wphase_Yvec; // 4 lanes rotate
		w_Yvec = w_Xvec * wphase_Yvec + w_Yvec * wphase_Xvec; // 4 lanes rotate
		w_Xvec = w_Xtmp;
	    }
	    mmax=istep;
	}
    }
}
