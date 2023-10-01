import std.math;
import std.complex;
import std.stdio;

class FFT {
	// Internal variables
	bool phasevec_exist = false;
	Complex!(float)[32] phasevec;

	// Public function
	void Fft(uint log2point, ref Complex!(float)[] xy_out, const Complex!(float)[] xy_in) {
		if (!phasevec_exist) {
			for (uint i=0; i<32; i++) {
				float point = 2<<i;
				phasevec[i] = complex(cos(-2.*PI/point), sin(-2.*PI/point));
			}
			phasevec_exist = true;
		}

		for (uint i=0; i < (1<<log2point); i++) {
			uint brev = i;
			brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1);
			brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2);
			brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4);
			brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8);
			brev = (brev >> 16) | (brev << 16);

			brev >>= 32-log2point;
			xy_out[brev] = xy_in[i];
		}

		// here begins the Danielson-Lanczos section
		uint n = 1<<log2point;
		uint l2pt = 0;
		uint mmax = 1;

		while (n > mmax) {
			uint istep = mmax<<1;
			//	float theta = -2*M_PI/istep
			//	float complex wphase_XY = cos(theta) + sin(theta)*I
			Complex!(float) wphase_XY = phasevec[l2pt];
			l2pt++;

			Complex!(float) w_XY = complex(float(1.), 0.);
			for (uint m=0; m < mmax; m++) {
				for (uint i=m; i < n; i += istep) {
					Complex!(float) tempXY  = w_XY * xy_out[i+mmax];
					xy_out[i+mmax]  = xy_out[i] - tempXY;
					xy_out[i     ] += tempXY;
				}
				w_XY *= wphase_XY; // rotate
			}
			mmax=istep;
		}
	}
}
