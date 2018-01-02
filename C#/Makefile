TARGET=Benchmark.exe

all:
	make -C ../C-fast-all-complex-float-types-opt-4x
	mcs -optimize -unsafe -r:System.Numerics.dll Fft.cs Benchmark.cs Params.cs Native.cs -out:$(TARGET)

clean:
	make -C ../C-fast-all-complex-float-types-opt-4x clean
	rm -f $(TARGET)

PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py
