TARGET=./dist/fft-benchmark
ARCH=x64 # arm, arm64, please consult: https://docs.microsoft.com/en-us/dotnet/core/rid-catalog#linux-rids 
DESTDIR=dist
VERSION=1.8.0

CC=gcc
CFLAGS =-Wall -O2 -funroll-all-loops -fPIC

PARAMS_CPU = -march=native
ifeq ($(shell uname -m), armv7l)
    PARAMS_CPU = -mcpu=cortex-a5 -mfpu=neon-fp16 
endif
CFLAGS+= $(PARAMS_CPU)

CFLAGS+=-DLOG2FFTSIZE=12
CFLAGS+=-DFFT_REPEAT=10000

# improve speed
CFLAGS+=-DMOD_SPEED

LDFLAGS=-shared
OBJS=fft.o

TARGET=benchmark

all: libfft
	dotnet build -c Release -p:VersionPrefix=$(VERSION)
	dotnet publish -c Release -o $(DESTDIR) --self-contained -r linux-$(ARCH) -p:PublishTrimmed=True -p:VersionPrefix=$(VERSION)

libfft: $(OBJS)
	$(CC) $(OBJS) $(LDFLAGS) -o libfft.so

clean:
	rm -fr $(TARGET) bin obj *.o *.so BenchmarkDotNet.Artifacts dist *.deb

package:
	dotnet msbuild /t:package /p:Arch=$(ARCH) -p:VersionPrefix=$(VERSION)

docker:
	dotnet msbuild /t:docker /p:Arch=$(ARCH) -p:VersionPrefix=$(VERSION)

PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py

test2:	test

fft.o: ../other_tests/C-tests/C-fast_double/fft.c
	$(CC) $(CFLAGS) -o $@ -c $<
