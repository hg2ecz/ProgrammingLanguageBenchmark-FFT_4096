# C# FFT benchmark

A radix-2 FFT in managed C#, measured against the same algorithm in C
(`../other_tests/C-tests/C-fast_double/fft.c`) and Rust (`rust/`), both called through P/Invoke.

## Requirements

- .NET SDK 10
- `gcc` and `make` (C library)
- Rust toolchain with `cargo` (Rust library)
- optional: `dpkg-deb` (`make package`), Docker (`make docker`)

## Build and run

```sh
make                      # builds libfft.so, libfftr.so and publishes to ./dist
./dist/fft-benchmark      # managed implementation only
./dist/fft-benchmark -m -n -u   # managed, C and Rust, with ratios
make test                 # 10 runs, averaged by ../avgcalc.py
```

| Option | Meaning |
|---|---|
| `-m`, `--managed` | managed C# implementation (default if nothing is selected) |
| `-n`, `--native` | C implementation (`libfft.so`) |
| `-u`, `--rust` | Rust implementation (`libfftr.so`) |
| `-d`, `--dotnet-benchmark` | all three with BenchmarkDotNet (always 4096 points) |
| `-r`, `--repeat <N>` | iterations (default 20000) |
| `-s`, `--size <N>` | log2 of the FFT size, 1..30 (default 12) |

The native libraries are compiled with `-march=native` / `-C target-cpu=native`, so a build only
runs reliably on CPUs that have the same instruction sets as the build machine.

## Packaging

```sh
make package              # fft-benchmark_<version>_<arch>.deb, installs to /opt/fft-benchmark
make docker               # Docker image fft-benchmark:<version>
make ARCH=arm64 package   # on an arm64 machine (native code is not cross-compiled)
```

The version comes from `<VersionPrefix>` in `fft-benchmark.csproj`.

## Source layout

| File | Contents |
|---|---|
| `FftManaged.cs` | managed FFT implementation |
| `NativeC.cs`, `NativeRust.cs` | P/Invoke wrappers for the native libraries |
| `Measurement.cs` | timing loop and result output shared by all implementations |
| `DotnetBenchmark.cs` | BenchmarkDotNet benchmarks |
| `Main.cs` | command-line handling |
