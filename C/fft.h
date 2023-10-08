#include <complex.h>

// step: N*log2(N)
void fft     (int log2point, float complex *restrict xy_out, const float complex *restrict xy_in);
