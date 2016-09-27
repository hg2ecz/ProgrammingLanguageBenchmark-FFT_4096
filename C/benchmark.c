#include <stdio.h>
#include "fft.h"

#include <time.h>
static struct timespec gstart;

static void timestart() {
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &gstart);
}

static double timeend() {
    struct timespec gend;
    unsigned long long nsec;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &gend);

    nsec = 1000ULL*1000*1000*(gend.tv_sec - gstart.tv_sec) + gend.tv_nsec - gstart.tv_nsec;
    return nsec/1000./1000.;
}

void printres(int i, double complex *d) {
    printf("%3d %16.9f %16.9f %16.9f\n", i, creal(d[i]), cimag(d[i]), cabs(d[i]));
}

#define SIZE (1<<LOG2FFTSIZE)

double complex xy[SIZE];
double complex xy_out_fft[SIZE];

// dft_ LOGSIZE --> dft_12
//#define FUNCTION_NAME_MAKE(name, num)  name ## num
//#define FUNCTION_NAME(name, num)  FUNCTION_NAME_MAKE(name, num)

int main() {
    int i;
    double eltime;
    printf("\n");

    for(i=0; i<SIZE/2; i++) xy[i]= 1.;
    for(   ; i<SIZE  ; i++) xy[i]=-1.;


// FFT
    timestart();
    for (i=0; i<FFT_REPEAT; i++) fft(LOG2FFTSIZE, xy_out_fft, xy);
    eltime = timeend();
    printf("%6d piece(s) of %d pt FFT;  %9.5f ms/piece\n", FFT_REPEAT, 1<<LOG2FFTSIZE, eltime/FFT_REPEAT);

    for(i=0; i<6; i++) {
	printres(i, xy_out_fft);
    }
    return 0;
}
