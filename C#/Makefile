TARGET=./dist/fft-benchmark

all:
	make -C ../C-tests/C-fast_double clean all
	gcc -shared -fPIC -o libfft.so ../C-tests/C-fast_double/fft.o
	# mcs -optimize -unsafe -r:System.Numerics.dll Fft.cs Benchmark.cs Params.cs Native.cs -out:$(TARGET)
	dotnet build -c Release
	dotnet publish -c Release -o dist --self-contained -r linux-x64

clean:
	make -C ../C-tests/C-fast_double clean
	rm -fr $(TARGET) bin obj *.so BenchmarkDotNet.Artifacts dist

PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py

test2:	test

