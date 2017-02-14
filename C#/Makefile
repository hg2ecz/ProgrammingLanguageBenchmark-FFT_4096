all:
	mcs -optimize -unsafe -r:System.Numerics.dll Benchmark.cs

clean:
	rm -f Benchmark.exe

PROG=Benchmark.exe
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py
