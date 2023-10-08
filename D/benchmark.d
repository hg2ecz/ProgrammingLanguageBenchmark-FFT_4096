import std.stdio;
import std.complex;
import std.datetime;
import fft;


int main() {
	const uint LOG2FFTSIZE = 12;
	const int FFT_REPEAT = 1000;

	const uint SIZE = (1<<LOG2FFTSIZE);
	static Complex!(float)[] xy;
	static Complex!(float)[] xy_out_fft;

	FFT fftobj = new FFT;
	xy.length = SIZE;
	xy_out_fft.length = SIZE;
	uint i;
	for (i=0; i<SIZE/2; i++) { xy[i]= complex(float(1.), 0.); }
	for (   ; i<SIZE  ; i++) { xy[i]= complex(float(-1.), 0.); }

	// FFT
	MonoTime startTime = MonoTime.currTime;
	for (i=0; i<FFT_REPEAT; i++) { fftobj.Fft(LOG2FFTSIZE, xy_out_fft, xy); }
	MonoTime endTime = MonoTime.currTime;
	Duration elapsedTime = endTime-startTime;
	writef("%8d piece(s) of %d pt FFT;    %.3f ms/piece\n\n", FFT_REPEAT, SIZE, elapsedTime.total!"nsecs"/1e6/FFT_REPEAT);

	writeln("bin        real             imag           absval");
	for (i=0; i<6; i++) {
		writefln("%3d %16.4f %16.4f %16.4f", i, xy_out_fft[i].re, xy_out_fft[i].im, abs(xy_out_fft[i]));
	}
	return 0;
}
