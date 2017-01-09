SRCS=main.fs
TARGET=benchmark.exe

all:
	fsharpc -o $(TARGET) $(SRCS)
	@chmod +x $(TARGET)

clean:
	rm -f $(TARGET)


PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py
