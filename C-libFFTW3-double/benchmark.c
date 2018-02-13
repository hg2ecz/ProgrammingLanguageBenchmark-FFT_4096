#include <time.h>
#include <stdio.h>
#include <complex.h>
#include <fftw3.h>

#define SIZE (1<<LOG2FFTSIZE)

double complex xy[SIZE];
double complex xy_out[SIZE];

int main() {
    double eltime;
    struct timespec gstart, gend;

// FFT
    fftw_plan plan_forward;
    plan_forward = fftw_plan_dft_1d(SIZE, xy, xy_out, FFTW_FORWARD, FFTW_ESTIMATE );

    for(int i=0; i<SIZE/2; i++) xy[i]= 1.;
    for(int i=SIZE/2; i<SIZE; i++) xy[i]=-1.;

    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &gstart);
    // only fft_execure repeated
    for (int i=0; i<FFT_REPEAT; i++) fftw_execute ( plan_forward );
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &gend);

    fftw_destroy_plan ( plan_forward );
    eltime = 1000.0*(gend.tv_sec - gstart.tv_sec) + (gend.tv_nsec - gstart.tv_nsec)/1000000.;
    printf("\n%6d piece(s) of %d pt FFT;  %9.5f ms/piece\n", FFT_REPEAT, 1<<LOG2FFTSIZE, eltime/FFT_REPEAT);

    for(int i=0; i<6; i++) {
	printf("%3d %16.9f %16.9f %16.9f\n", i, creal(xy_out[i]), cimag(xy_out[i]), cabs(xy_out[i]));
    }
    return 0;
}
