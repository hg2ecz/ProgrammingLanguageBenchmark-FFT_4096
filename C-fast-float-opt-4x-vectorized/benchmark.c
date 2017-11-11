#include <time.h>
#include <stdio.h>
#include <string.h>
#include "fft.h"
#ifdef CHECKFFT
# include "dft_test.h"
#endif

#define SIZE (1<<LOG2FFTSIZE)

struct _complexblock xy;
struct _complexblock *xy_out;

int main(int argc, char **argv) {
    int i;
    double eltime;
    struct timespec gstart, gend;

    //for(i=0; i<SIZE/2; i++) { xy.re[i]= 1.; xy.im[i]= 0.; }
    //for(   ; i<SIZE  ; i++) { xy.re[i]=-1.; xy.im[i]= 0.; }
    for(i=0; i<SIZE; i++) { xy.re[i]= 1.+i/1000.; xy.im[i]= i/2000.; }

// FFT
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &gstart);
    for (i=0; i<FFT_REPEAT; i++) xy_out = fft(LOG2FFTSIZE, xy);
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &gend);

    eltime = 1000.0*(gend.tv_sec - gstart.tv_sec) + (gend.tv_nsec - gstart.tv_nsec)/1000000.;
    printf("\n%6d piece(s) of %d pt FFT;  %9.5f ms/piece\n", FFT_REPEAT, 1<<LOG2FFTSIZE, eltime/FFT_REPEAT);

#ifdef CHECKFFT
    if (argc > 1 && !strcmp(argv[1], "check")) {
	fprintf(stderr, "Check with dft ...\n");
	dft_test(xy_out, &xy, LOG2FFTSIZE, 1e-6); // compare & relative error
    }
#endif
    for(i=0; i<6; i++) {
	printf("%3d %16.4f %16.4f\n", i, xy_out->re[i], xy_out->im[i]);
    }
    return 0;
}
