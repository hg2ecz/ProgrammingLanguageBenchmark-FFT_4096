#define INTMUL (19+5)
#define INTMULSAMPLE (19-4)

struct _sample {
    int i;
    int q;
};
// step: N*log2(N)
void fft     (int log2point, struct _sample *restrict xy_out, const struct _sample *restrict xy_in);
