#include <time.h>
#include <stdio.h>
#include "fft.h"

#ifdef CHECKFFT
# include "dft_test.h"
#endif

#define SIZE (1<<LOG2FFTSIZE)

COMPLEX_TYPE xy[SIZE];
COMPLEX_TYPE xy_out[SIZE];

int main() {
    double eltime;
    struct timespec gstart, gend;

    for(int i=0; i<SIZE/2; i++) xy[i]= 1.;
    for(int i=SIZE/2; i<SIZE; i++) xy[i]=-1.;

// FFT
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &gstart);
    for (int i=0; i<FFT_REPEAT; i++) fft(LOG2FFTSIZE, xy_out, xy);
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &gend);

    eltime = 1000.0*(gend.tv_sec - gstart.tv_sec) + (gend.tv_nsec - gstart.tv_nsec)/1000000.;
    printf("\n%6d piece(s) of %d pt FFT;  %9.5f ms/piece\n", FFT_REPEAT, 1<<LOG2FFTSIZE, eltime/FFT_REPEAT);

#ifdef CHECKFFT
    dft_test(xy_out, xy, LOG2FFTSIZE, 1e-6); // compare & relative error
#endif
    for(int i=0; i<6; i++) {
	printf("%3d %16.4f %16.4f\n", i, creal(xy_out[i]), cimag(xy_out[i]));
    }
    return 0;
}
