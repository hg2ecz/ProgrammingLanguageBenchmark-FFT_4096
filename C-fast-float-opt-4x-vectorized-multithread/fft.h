#include "float_type.h"

#define SIZE (1<<LOG2FFTSIZE)

struct _complexblock {
    FLOAT_TYPE re[SIZE];
    FLOAT_TYPE im[SIZE];
};
// step: N*log2(N)
void fft_init();
struct _complexblock *fft(int log2point,const struct _complexblock xy_in);
