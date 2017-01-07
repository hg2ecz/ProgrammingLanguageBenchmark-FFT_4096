#define INTMUL (19+5)
#define INTMULSAMPLE (19-4)

struct _sample {
    int fract[1<<LOG2FFTSIZE][2];
};
// step: N*log2(N)
void fft     (int log2point, struct _sample *restrict xy_out, const struct _sample *restrict xy_in);
