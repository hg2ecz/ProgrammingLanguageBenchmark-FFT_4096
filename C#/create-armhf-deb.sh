dotnet publish -r linux-arm -c Release -o packaging/opt/fft-benchmark
mkdir -p packaging/opt/fft-benchmark/C-fast-all-complex-float-types-opt-4x
cp ../C-fast-all-complex-float-types-opt-4x/fft.so packaging/opt/fft-benchmark/C-fast-all-complex-float-types-opt-4x
dpkg-deb -b packaging fft-benchmark-1.1.armhf.deb
