dotnet publish -r linux-arm -c Release -o packaging/opt/fft-benchmark
dpkg-deb -b packaging fft-benchmark-1.1.armhf.deb
