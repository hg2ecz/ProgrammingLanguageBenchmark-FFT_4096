dotnet publish -r linux-x64 -c Release -o packaging/opt/fft-benchmark
dpkg-deb -b packaging fft-benchmark-1.0.armhf.deb