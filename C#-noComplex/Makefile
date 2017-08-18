TARGET=Benchmark.exe

all:
	mcs -optimize -unsafe Fft.cs Benchmark.cs -out:$(TARGET)

clean:
	rm -f $(TARGET)

PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py
