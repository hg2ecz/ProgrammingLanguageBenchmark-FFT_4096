FC=f95
FFLAGS = -Ofast -funroll-all-loops

#CFLAGS+=-DLOG2FFTSIZE=12
#CFLAGS+=-DFFT_REPEAT=1000

LDFLAGS=-s
OBJS=fft.o benchmark.o

TARGET=benchmark


all: $(OBJS)
	$(FC) $(OBJS) $(LDFLAGS) -o $(TARGET)

clean:
	rm -f $(OBJS) $(TARGET) *.mod

PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py
