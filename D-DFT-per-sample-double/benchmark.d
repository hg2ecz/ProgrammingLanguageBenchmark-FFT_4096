import std.stdio;
import std.datetime;
import dft;

const uint LOG2FFTSIZE = 12;
const uint FFT_REPEAT = 10;

const uint SIZE = (1<<LOG2FFTSIZE);
cdouble[SIZE] xy;
cdouble[SIZE] xy_out_fft = 0+0i; // init with zero else default nan

int main() {
    uint i;
    for (i=0; i<SIZE/2; i++) { xy[i]=  1.+0i; }
    for (   ; i<SIZE  ; i++) { xy[i]= -1.+0i; }

// FFT
    dft.Dft_init(SIZE);
    MonoTime startTime = MonoTime.currTime;
    for (i=0; i<FFT_REPEAT; i++) {
	for (uint j=0; j<SIZE; j++) {
	    dft.Dft_sample(xy_out_fft, xy[j]);
	}
    }
    MonoTime endTime = MonoTime.currTime;
    Duration elapsedTime = endTime-startTime;

    writef("%8d piece(s) of %d pt FFT;    %.3f ms/piece\n\n", FFT_REPEAT, SIZE, elapsedTime.total!"nsecs"/1e6/FFT_REPEAT);

    for (i=0; i<6; i++) {
	//writeln(i, xy_out_fft[i]);
	writefln("%f %f", xy_out_fft[i].re, xy_out_fft[i].im);
    }
    return 0;
}
