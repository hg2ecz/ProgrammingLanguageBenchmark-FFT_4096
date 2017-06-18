#define MULTYPE int
#define INTMUL (4)
#define INTMULSAMPLE (3)

struct _sample {
    short i;
    short q;
};
// step: N*log2(N)
void fft     (int log2point, struct _sample *restrict xy_out, const struct _sample *restrict xy_in);
