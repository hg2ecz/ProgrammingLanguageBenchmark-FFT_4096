#include <omp.h>
#include <stdio.h>
#include <complex.h>
#include <sys/time.h>
#include <fftw3.h>

#define SIZE (1<<LOG2FFTSIZE)

#define MAXTHREAD 64
float complex xy[MAXTHREAD][SIZE];
float complex xy_out[MAXTHREAD][SIZE];

int main() {
    double eltime;
    struct timeval tstart, tend;

// FFT
    for (int j=0; j<MAXTHREAD; j++) {
	for(int i=0; i<SIZE/2; i++) xy[j][i]=  j+1.;
	for(int i=SIZE/2; i<SIZE; i++) xy[j][i]= -j-1.;
    }

// FFT
    fftwf_plan plan_forward;

    gettimeofday(&tstart, NULL);

    int nthreads, tid;
    #pragma omp parallel private (nthreads, tid, plan_forward)
    {
	nthreads = omp_get_num_threads(); // number of all thread
	tid = omp_get_thread_num(); // thread id
	if (tid == 0) printf("Number of threads = %d\n", nthreads);
	if (tid < MAXTHREAD) { // vectorsize !!!
	    plan_forward = fftwf_plan_dft_1d(SIZE, xy[tid], xy_out[tid], FFTW_FORWARD, FFTW_ESTIMATE );
	    for (int i=0; i<FFT_REPEAT/nthreads; i++) fftwf_execute ( plan_forward );
	    fftwf_destroy_plan ( plan_forward );
	}
    }

    gettimeofday(&tend, NULL);
    eltime = 1000.*(tend.tv_sec - tstart.tv_sec) + (tend.tv_usec - tstart.tv_usec)/1000.;
    printf("\n%6d piece(s) of %d pt FFT;  %9.5f ms/piece => %4.1f FFT/sec\n",
         FFT_REPEAT, 1<<LOG2FFTSIZE, eltime/FFT_REPEAT, 1000.*FFT_REPEAT/eltime);

    for(int i=0; i<6; i++) {
	printf("%3d %16.9f %16.9f %16.9f\n", i, creal(xy_out[0][i]), cimag(xy_out[0][i]), cabs(xy_out[0][i]));
    }
    puts("");
    for(int i=0; i<6; i++) {
	printf("%3d %16.9f %16.9f %16.9f\n", i, creal(xy_out[1][i]), cimag(xy_out[1][i]), cabs(xy_out[1][i]));
    }
    return 0;
}
