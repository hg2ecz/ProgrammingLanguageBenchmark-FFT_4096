#include <time.h>
#include <iostream>
#include <vector>
#include <memory>

#include "fft.h"

#ifndef LOG2FFTSIZE
#define LOG2FFTSIZE 12
#endif

#ifndef FFT_REPEAT
#define FFT_REPEAT 12000
#endif

#define SIZE (1 << LOG2FFTSIZE)

int main()
{
    std::vector<std::complex<float>> xy(SIZE);

    float eltime;
    struct timespec gstart, gend;

    for (int i = 0; i < SIZE / 2; i++)
        xy[i] = 1.;

    for (int i = SIZE / 2; i < SIZE; i++)
        xy[i] = -1.;

    // FFT
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &gstart);

    auto ff = std::make_unique<fft>();
    std::vector<std::complex<float>> xy_out;

    for (int i = 0; i < FFT_REPEAT; i++)
    {
        xy_out = ff->calc(LOG2FFTSIZE, xy);
    }

    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &gend);
    eltime = 1000. * (gend.tv_sec - gstart.tv_sec) + (gend.tv_nsec - gstart.tv_nsec) / 1000000.;
    
    printf("\n%6d piece(s) of %d pt FFT;  %9.5f ms/piece\n", FFT_REPEAT, 1 << LOG2FFTSIZE, eltime / FFT_REPEAT);

    for (int i = 0; i < 6; i++)
    {
        printf("%3d %16.9f %16.9f %16.9f\n", i, xy_out[i].real(), xy_out[i].imag(), abs(xy_out[i]));
    }
    return 0;
}
