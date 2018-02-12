#include <time.h>
#include <stdio.h>
#include "dft.h"

#define SIZE (1<<LOG2FFTSIZE)

float complex xy[SIZE];
float complex xy_out[SIZE];

int main() {
    double eltime;
    struct timespec gstart, gend;

    for(int i=0; i<SIZE/2; i++) xy[i]= 1.;
    for(int i=SIZE/2; i<SIZE; i++) xy[i]=-1.;

// DFT
    dft_init(SIZE);
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &gstart);
    for (int i=0; i<FFT_REPEAT; i++) dft(xy_out, xy);
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &gend);

    eltime = 1000.0*(gend.tv_sec - gstart.tv_sec) + (gend.tv_nsec - gstart.tv_nsec)/1000000.;
    printf("\n%6d piece(s) of %d pt DFT;  %9.5f ms/piece\n", FFT_REPEAT, SIZE, eltime/FFT_REPEAT);

    for(int i=0; i<6; i++) {
	printf("%3d %16.9f %16.9f %16.9f\n", i, creal(xy_out[i]), cimag(xy_out[i]), cabs(xy_out[i]));
    }
    return 0;
}
