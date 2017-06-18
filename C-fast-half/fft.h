#include <complex.h>

// step: N*log2(N)
void fft     (int log2point, _Complex _Float16 *restrict xy_out, const _Complex _Float16 *restrict xy_in);
