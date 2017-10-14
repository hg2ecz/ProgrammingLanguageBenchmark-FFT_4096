CC=gcc
CFLAGS =-Wall -Ofast -funroll-all-loops -fPIC

PARAMS_CPU = -march=native
ifeq ($(shell uname -m), armv6l)
    PARAMS_CPU = -march=native -mfp16-format=ieee
endif
ifeq ($(shell uname -m), armv7l)
    PARAMS_CPU = -mcpu=cortex-a5 -mfpu=neon-fp16 -mfp16-format=ieee
endif
CFLAGS+= $(PARAMS_CPU)

CFLAGS+=-DLOG2FFTSIZE=12
CFLAGS+=-DFFT_REPEAT=1000

# improve speed
CFLAGS+=-DMOD_SPEED

LDFLAGS=-lm -s
OBJS=dft_test.o fft.o benchmark.o

TARGET=benchmark

all: $(OBJS) shlib
	$(CC) $(OBJS) $(LDFLAGS) -o $(TARGET)

shlib:	fft.o
	$(CC) -shared -o fft.so fft.o

clean:
	rm -rf $(OBJS) $(TARGET) fft.so

PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py
