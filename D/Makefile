DCC=gdc
CFLAGS =-Wall -Ofast -funroll-all-loops -g

#CFLAGS+=-DLOG2FFTSIZE=12
#CFLAGS+=-DFFT_REPEAT=1000

LDFLAGS=-s
OBJS=ifft.o fft.o benchmark.o main.o

TARGET=benchmark

%.o: %.d
	$(DCC) $(CFLAGS) -c $<

all: $(OBJS)
	$(DCC) $(OBJS) $(LDFLAGS) -o $(TARGET)

clean:
	rm -rf $(OBJS) $(TARGET)

PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py
