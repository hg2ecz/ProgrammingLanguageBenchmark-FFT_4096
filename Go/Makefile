GOCC=gccgo
CFLAGS =-Wall -Ofast -funroll-all-loops

PARAMS_CPU = -march=native
ifeq ($(shell uname -m), armv7l)
    PARAMS_CPU = -mcpu=cortex-a5 -mfpu=neon-fp16 
endif
CFLAGS+= $(PARAMS_CPU)

#CFLAGS+=-DLOG2FFTSIZE=12
#CFLAGS+=-DFFT_REPEAT=1000

LDFLAGS=-lm -s
OBJS=fft.o benchmark.o

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
