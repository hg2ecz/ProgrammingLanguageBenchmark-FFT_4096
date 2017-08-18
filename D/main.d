import benchmark;
import ifft;
import fft;

int main()
{
    FFT fft = new FFT();
    assert(fft !is null);

    Benchmark benchmark = new Benchmark(fft);
    assert(benchmark !is null);

    benchmark.Execute();

    return 0;
}
