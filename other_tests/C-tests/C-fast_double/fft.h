#include <complex.h>

// step: N*log2(N)
void fft     (int log2point, double complex *restrict xy_out, const double complex *restrict xy_in);
