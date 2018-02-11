#include <omp.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include "fft.h"
#ifdef CHECKFFT
# include "dft_test.h"
#endif

#define SIZE (1<<LOG2FFTSIZE)

#define MAXTHREAD 64
struct _complexblock xy[MAXTHREAD];
struct _complexblock xy_out[MAXTHREAD];

int main(int argc, char **argv) {
    int i;
    double eltime;
    struct timeval tstart, tend;

    //for(i=0; i<SIZE/2; i++) { xy.re[i]= 1.; xy.im[i]= 0.; }
    //for(   ; i<SIZE  ; i++) { xy.re[i]=-1.; xy.im[i]= 0.; }
    for (int j=0; j<MAXTHREAD; j++) {
	for(i=0; i<SIZE; i++) { xy[j].re[i]= 1.+(j+1)*i/1000.; xy[j].im[i]= (j+1)*i/2000.; }
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
           for (i=0; i<FFT_REPEAT/nthreads; i++)  fft(LOG2FFTSIZE, &xy_out[tid], xy[tid]);
    }

    gettimeofday(&tend, NULL);
    eltime = 1000.*(tend.tv_sec - tstart.tv_sec) + (tend.tv_usec - tstart.tv_usec)/1000.;
    printf("\n%6d piece(s) of %d pt FFT;  %9.5f ms/piece => %4.1f FFT/sec\n",
         FFT_REPEAT, 1<<LOG2FFTSIZE, eltime/FFT_REPEAT, 1000.*FFT_REPEAT/eltime);

#ifdef CHECKFFT
    if (argc > 1 && !strcmp(argv[1], "check")) {
	fprintf(stderr, "Check with dft ...\n");
	dft_test(&xy_out[0], &xy[0], LOG2FFTSIZE, 1e-6); // compare & relative error
	dft_test(&xy_out[1], &xy[1], LOG2FFTSIZE, 1e-6); // compare & relative error
	dft_test(&xy_out[2], &xy[2], LOG2FFTSIZE, 1e-6); // compare & relative error
	dft_test(&xy_out[3], &xy[3], LOG2FFTSIZE, 1e-6); // compare & relative error
    }
#endif
    for(i=0; i<6; i++) {
	printf("%3d %16.4f %16.4f\n", i, xy_out[0].re[i], xy_out[0].im[i]);
    }
    puts("");
    for(i=0; i<6; i++) {
	printf("%3d %16.4f %16.4f\n", i, xy_out[1].re[i], xy_out[1].im[i]);
    }
    return 0;
}
