#include <complex>

class fft {
    // Internal variables
    private:
	std::complex<double> phasevec[32];

    public:
	fft(void);
	// step: N*log2(N)
	void calc(int log2point, std::complex<double> *xy_out, const std::complex<double> *xy_in);
};
