#include <complex>
#include <vector>

class fft {
private:
    // Internal variables
	std::vector<std::complex<float>> phasevec;

public:
	fft();
	// step: N*log2(N)
	std::vector<std::complex<float>> calc(int log2point, const std::vector<std::complex<float>>& xy_in);
};
