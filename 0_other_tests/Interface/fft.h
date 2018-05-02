#include "float_type.h"
#include <complex.h>

#define SIZE (1<<LOG2FFTSIZE)

struct _complexblock {
    FLOAT_TYPE re[SIZE];
    FLOAT_TYPE im[SIZE];
};
// step: N*log2(N)
struct _complexblock *fft_vector_io(int log2point,const struct _complexblock xy_in);
void fft_complex_io(int log2point, complex float *xy_out, const complex float *xy_in);
