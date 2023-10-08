#include "float_type.h"

#define SIZE (1<<LOG2FFTSIZE)

struct _complexblock {
    FLOAT_TYPE re[SIZE];
    FLOAT_TYPE im[SIZE];
};
// step: N*log2(N)
void fft_init();
void fft(int log2point, struct _complexblock *xy_out, const struct _complexblock xy_in);
