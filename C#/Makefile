TARGET=./dist/fft-benchmark
ARCH=x64 # arm, arm64, please consult: https://docs.microsoft.com/en-us/dotnet/core/rid-catalog#linux-rids 
DESTDIR=dist
VERSION=1.6.0

all:
	make -C ../C-tests/C-fast_double clean all
	gcc -shared -fPIC -o libfft.so ../C-tests/C-fast_double/fft.o
	# mcs -optimize -unsafe -r:System.Numerics.dll Fft.cs Benchmark.cs Params.cs Native.cs -out:$(TARGET)
	dotnet build -c Release -p:VersionPrefix=$(VERSION)
	dotnet publish -c Release -o $(DESTDIR) --self-contained -r linux-$(ARCH) -p:PublishTrimmed=True -p:VersionPrefix=$(VERSION)

clean:
	make -C ../C-tests/C-fast_double clean
	rm -fr $(TARGET) bin obj *.so BenchmarkDotNet.Artifacts dist *.deb

package:
	dotnet msbuild /t:package /p:Arch=$(ARCH) -p:VersionPrefix=$(VERSION)

docker:
	dotnet publish -c Release -o dist --self-contained -r linux-$(ARCH) -p:PublishTrimmed=True -p:VersionPrefix=$(VERSION)
	docker build -t fft-benchmark:$(VERSION) .

PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py

test2:	test

