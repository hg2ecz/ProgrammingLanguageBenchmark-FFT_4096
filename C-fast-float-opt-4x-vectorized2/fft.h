#include <complex.h>
typedef float FLOAT_TYPE;
typedef float FLOAT_VFO_TYPE;

// step: N*log2(N)
void fft_vec(int log2point, FLOAT_TYPE *out_re, FLOAT_TYPE *out_im, const FLOAT_TYPE *in_re, const FLOAT_TYPE *in_im);
void fft_cplx(int log2point, complex float *xy_out, const complex float *xy_in);
