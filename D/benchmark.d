import std.stdio;
import std.datetime;
import ifft;

const uint LOG2FFTSIZE = 12;
const int FFT_REPEAT = 1000;

const uint SIZE = (1<<LOG2FFTSIZE);
static cdouble[SIZE] xy;
static cdouble[SIZE] xy_out_fft;

class Benchmark {
	IFFT fft;

	this(IFFT fft)
	{
		this.fft = fft;
	}

	int Execute() {
    		uint i;
    		for (i=0; i<SIZE/2; i++) { xy[i]= (1.+0i); }
    		for (   ; i<SIZE  ; i++) { xy[i]= (-1.+0i); }

		// FFT
    		MonoTime startTime = MonoTime.currTime;
    		for (i=0; i<FFT_REPEAT; i++) { this.fft.Fft(LOG2FFTSIZE, xy_out_fft, xy); }
    		MonoTime endTime = MonoTime.currTime;
    		Duration elapsedTime = endTime-startTime;

    		writef("%8d piece(s) of %d pt FFT;    %.3f ms/piece\n\n", FFT_REPEAT, SIZE, elapsedTime.total!"nsecs"/1e6/FFT_REPEAT);

    		for (i=0; i<6; i++) {
			writefln("%f %f", xy_out_fft[i].re, xy_out_fft[i].im);
    		}
    		return 0;
	}
}
