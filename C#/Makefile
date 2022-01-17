TARGET=./dist/fft-benchmark
ARCH=x64

all:
	make -C ../C-tests/C-fast_double clean all
	gcc -shared -fPIC -o libfft.so ../C-tests/C-fast_double/fft.o
	# mcs -optimize -unsafe -r:System.Numerics.dll Fft.cs Benchmark.cs Params.cs Native.cs -out:$(TARGET)
	dotnet build -c Release
	dotnet publish -c Release -o dist --self-contained -r linux-$(ARCH) -p:PublishTrimmed=True

clean:
	make -C ../C-tests/C-fast_double clean
	rm -fr $(TARGET) bin obj *.so BenchmarkDotNet.Artifacts dist *.deb

package:
	dotnet msbuild /t:package /p:Arch=$(ARCH)

docker:
	dotnet publish -c Release -o dist --self-contained -r linux-$(ARCH) -p:PublishTrimmed=True
	docker build -t fft-benchmark:latest .

PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py

test2:	test

