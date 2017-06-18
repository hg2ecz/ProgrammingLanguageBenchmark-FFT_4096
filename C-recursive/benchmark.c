#include <time.h>
#include <stdio.h>
#include <string.h>
#include "fft_recursive.h"

#define SIZE (1<<LOG2FFTSIZE)

double complex xy[SIZE];
double complex xy_out[SIZE];
double complex xy_tmp[SIZE];

int main() {
    int i;
    double eltime;
    struct timespec gstart, gend;

    for(i=0; i<SIZE/2; i++) xy[i]= 1.;
    for(   ; i<SIZE  ; i++) xy[i]=-1.;

// FFT
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &gstart);
    for (i=0; i<FFT_REPEAT; i++) {
	memcpy(xy_out, xy, SIZE*sizeof(double complex));
	fft_recursive(xy_out, xy_tmp, 1<<LOG2FFTSIZE, 1);
    }
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &gend);

    eltime = 1000.0*(gend.tv_sec - gstart.tv_sec) + (gend.tv_nsec - gstart.tv_nsec)/1000000.;
    printf("\n%6d piece(s) of %d pt FFT;  %9.5f ms/piece\n", FFT_REPEAT, 1<<LOG2FFTSIZE, eltime/FFT_REPEAT);

    for(i=0; i<6; i++) {
	printf("%3d %16.9f %16.9f %16.9f\n", i, creal(xy_out[i]), cimag(xy_out[i]), cabs(xy_out[i]));
    }
    return 0;
}
