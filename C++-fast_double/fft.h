#include <complex>
#include <vector>

class fft {
private:
    // Internal variables
	std::vector<std::complex<double>> phasevec;

public:
	fft();
	// step: N*log2(N)
	std::vector<std::complex<double>> calc(int log2point, const std::vector<std::complex<double>>& xy_in);
};
