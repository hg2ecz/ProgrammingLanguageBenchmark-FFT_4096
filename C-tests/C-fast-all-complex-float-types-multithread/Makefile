CC=gcc
CFLAGS =-Wall -Ofast -funroll-all-loops -fPIC -fopenmp

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

LDFLAGS=-lm -s -lgomp
OBJS=fft.o benchmark.o

TARGET=benchmark

all: $(OBJS) shlib
	$(CC) $(OBJS) $(LDFLAGS) -o $(TARGET)

shlib:	fft.o
	$(CC) -shared -o fft.so fft.o

clean:
	rm -rf $(OBJS) $(TARGET) fft.so

PROG=$(TARGET)
test:
	./$(PROG) check
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py
