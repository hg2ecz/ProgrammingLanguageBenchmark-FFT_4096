struct _sample {
    float i;
    float q;
};
// step: N*log2(N)
void fft     (int log2point, struct _sample *restrict xy_out, const struct _sample *restrict xy_in);
