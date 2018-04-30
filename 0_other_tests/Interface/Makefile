CC=gcc
CFLAGS =-Wall -Ofast -funroll-all-loops

PARAMS_CPU = -march=native
ifeq ($(shell uname -m), armv6l)
    PARAMS_CPU = -march=native -mfp16-format=ieee
endif
ifeq ($(shell uname -m), armv7l)
    PARAMS_CPU = -mcpu=cortex-a5 -mfpu=neon-fp16 -mfp16-format=ieee
endif
CFLAGS+= $(PARAMS_CPU)

CFLAGS+=-DLOG2FFTSIZE=12
CFLAGS+=-DFFT_REPEAT=100000

# improve speed
CFLAGS+=-DMOD_SPEED

LDFLAGS=-lm -s
OBJS=fft-vector_io.o fft-complex_io.o benchmark.o

TARGET=benchmark

all: $(OBJS)
	$(CC) $(OBJS) $(LDFLAGS) -o $(TARGET)

clean:
	rm -rf $(OBJS) $(TARGET)

PROG=$(TARGET)
test:
	./$(PROG) check
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py
