interface IFFT {
 	void Fft(uint log2point, ref cdouble[4096] xy_out, const cdouble[4096] xy_in);
}
