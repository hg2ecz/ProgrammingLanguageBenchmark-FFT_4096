public class Benchmark {

    static int LOG2FFTSIZE = 12;
    static int FFT_REPEAT = 1000;

    public static void main(String[] args){
	int i;
	//double eltime;
	int SIZE = 1<<LOG2FFTSIZE;
	double[][] xy = new double[SIZE][2];
	double[][] xy_out_fft = new double[SIZE][2];

	for(i=0; i<SIZE/2; i++) { xy[i][0]=  1.; xy[i][1]= 0.; }
	for(   ; i<SIZE  ; i++) { xy[i][0]= -1.; xy[i][1]= 0.; }

// warm up ... JIT activating
	Fft fft = new Fft();
	for (i=0; i<FFT_REPEAT; i++) fft.fft(LOG2FFTSIZE, xy_out_fft, xy);
// FFT & time measurement
	long starttime = System.nanoTime();
	for (i=0; i<FFT_REPEAT; i++) fft.fft(LOG2FFTSIZE, xy_out_fft, xy);
	double eltime = (System.nanoTime() - starttime)/1000000.;
	System.out.format("%6d piece(s) of %d pt FFT;  %9.5f ms/piece\n", FFT_REPEAT, 1<<LOG2FFTSIZE, eltime/FFT_REPEAT);

	for(i=0; i<6; i++) {
	    System.out.format("%d\t%f\t%f\n", i, xy_out_fft[i][0], xy_out_fft[i][1]);
	}
    }
}
