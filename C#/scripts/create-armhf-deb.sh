dotnet publish -r linux-arm -c Release -o packaging/opt/fft-benchmark
dpkg-deb -b packaging fft-benchmark.armhf.deb