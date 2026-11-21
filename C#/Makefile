# C# FFT benchmark (managed C# + native C and Rust implementations via P/Invoke)
#
#   make            build native libraries and publish a self-contained app to ./dist
#   make test       run the benchmark 10 times and print the average (../avgcalc.py)
#   make package    build a .deb package
#   make docker     build a Docker image
#   make clean

PROJECT := fft-benchmark.csproj
VERSION := 1.9.1.2

# .NET runtime identifier architecture: x64 or arm64 (defaults to the build machine)
ARCH ?= $(if $(filter aarch64 arm64,$(shell uname -m)),arm64,x64)
DEB_ARCH := $(if $(filter arm64,$(ARCH)),arm64,amd64)

DESTDIR ?= dist
PKGROOT := obj/deb
DEB := fft-benchmark_$(VERSION)_$(DEB_ARCH).deb
IMAGE := fft-benchmark:$(VERSION)

PUBLISH := dotnet publish $(PROJECT) -c Release -p:VersionPrefix=$(VERSION) -r linux-$(ARCH) --self-contained -p:PublishTrimmed=false

# Native C implementation, shared with the C benchmark
C_SRC := ../other_tests/C-tests/C-fast_double/fft.c
CC ?= gcc
CFLAGS := -Wall -O2 -funroll-all-loops -fPIC -DMOD_SPEED
ifeq ($(shell uname -m), armv7l)
    CFLAGS += -mcpu=cortex-a5 -mfpu=neon-fp16
else
    CFLAGS += -march=native
endif

.PHONY: all native publish test test2 package docker clean

all: publish

native: libfft.so libfftr.so

libfft.so: $(C_SRC)
	$(CC) $(CFLAGS) -shared -o $@ $<

libfftr.so: rust/Cargo.toml rust/src/lib.rs
	cd rust && ./build.sh
	cp rust/target/release/libfftr.so $@

publish: native
	$(PUBLISH) -o $(DESTDIR)

test: publish
	./$(DESTDIR)/fft-benchmark
	@for i in 1 2 3 4 5 6 7 8 9 10; do ./$(DESTDIR)/fft-benchmark; done | ../avgcalc.py

test2: test

package: native
	rm -rf $(PKGROOT)
	mkdir -p $(PKGROOT)/DEBIAN $(PKGROOT)/usr/bin
	sed -e 's/%%VERSION%%/$(VERSION)/g' -e 's/%%ARCH%%/$(DEB_ARCH)/g' packaging/DEBIAN/control.in > $(PKGROOT)/DEBIAN/control
	$(PUBLISH) -o $(PKGROOT)/opt/fft-benchmark
	find $(PKGROOT) -type d -exec chmod 755 {} +
	find $(PKGROOT) -type f -exec chmod 644 {} +
	chmod 755 $(PKGROOT)/opt/fft-benchmark/fft-benchmark
	ln -s /opt/fft-benchmark/fft-benchmark $(PKGROOT)/usr/bin/fft-benchmark
	dpkg-deb --root-owner-group --build $(PKGROOT) $(DEB)

docker: publish
	docker build -t $(IMAGE) .

clean:
	rm -rf bin obj $(DESTDIR) BenchmarkDotNet.Artifacts rust/target *.so *.o *.deb
