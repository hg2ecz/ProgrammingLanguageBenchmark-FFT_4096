#include <complex.h>
#include "float_type.h"

// step: N*log2(N)
void fft     (int log2point, COMPLEX_TYPE *restrict xy_out, const COMPLEX_TYPE *restrict xy_in);
