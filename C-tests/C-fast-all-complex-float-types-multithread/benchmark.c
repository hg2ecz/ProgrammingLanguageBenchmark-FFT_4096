#include <omp.h>
#include <stdio.h>
#include "fft.h"
#include <sys/time.h>

#ifdef CHECKFFT
# include "dft_test.h"
#endif

#define SIZE (1<<LOG2FFTSIZE)

#define MAXTHREAD 64
COMPLEX_TYPE xy[MAXTHREAD][SIZE];
COMPLEX_TYPE xy_out[MAXTHREAD][SIZE];

int main() {
    double eltime;
    struct timeval tstart, tend;

    for (int j=0; j<MAXTHREAD; j++) {
	for(int i=0; i<SIZE/2; i++) xy[j][i]=  j+1.;
	for(int i=SIZE/2; i<SIZE; i++) xy[j][i]= -j-1.;
    }

// FFT
    fft_init();
    gettimeofday(&tstart, NULL);

    int nthreads, tid;
    #pragma omp parallel private (nthreads, tid)
    {
	nthreads = omp_get_num_threads(); // number of all thread
	tid = omp_get_thread_num(); // thread id
	if (tid == 0) printf("Number of threads = %d\n", nthreads);
	if (tid < MAXTHREAD) // vectorsize !!!
           for (int i=0; i<FFT_REPEAT/nthreads; i++) fft(LOG2FFTSIZE, xy_out[tid], xy[tid]);
    }

    gettimeofday(&tend, NULL);
    eltime = 1000.*(tend.tv_sec - tstart.tv_sec) + (tend.tv_usec - tstart.tv_usec)/1000.;
    printf("\n%6d piece(s) of %d pt FFT;  %9.5f ms/piece => %4.1f FFT/sec\n",
         FFT_REPEAT, 1<<LOG2FFTSIZE, eltime/FFT_REPEAT, 1000.*FFT_REPEAT/eltime);

#ifdef CHECKFFT
    dft_test(xy_out, xy, LOG2FFTSIZE, 1e-6); // compare & relative error
#endif
    for(int i=0; i<6; i++) {
	printf("%3d %16.4f %16.4f\n", i, creal(xy_out[0][i]), cimag(xy_out[0][i]));
    }
    puts("");
    for(int i=0; i<6; i++) {
	printf("%3d %16.4f %16.4f\n", i, creal(xy_out[1][i]), cimag(xy_out[1][i]));
    }
    return 0;
}
