#!/usr/bin/env sh

mkdir -p packaging/usr/bin
cp ~/.local/bin/fft-hs packaging/usr/bin

if [ -x /usr/bin/upx ]; then
    upx -9 packaging/usr/bin/fft-hs
fi

dpkg-deb -b packaging fft-hs.armhf.deb

