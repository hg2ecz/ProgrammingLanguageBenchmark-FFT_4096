GOCC=gccgo
CFLAGS =-Wall -Ofast -funroll-all-loops -g

#CFLAGS+=-DLOG2FFTSIZE=12
#CFLAGS+=-DFFT_REPEAT=1000

LDFLAGS=-s
OBJS=dft.o benchmark.o

TARGET=benchmark

%.o: %.go
	$(GOCC) $(CFLAGS) -c $<

all: $(OBJS)
	$(GOCC) $(OBJS) $(LDFLAGS) -o $(TARGET)

clean:
	rm -rf $(OBJS) $(TARGET)

PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py
