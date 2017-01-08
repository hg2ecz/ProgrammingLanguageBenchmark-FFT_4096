CC=gcc

CFLAGS =-Wall -Ofast -funroll-all-loops

# ARM and x86
#CFLAGS+=-mcpu=cortex-a5 -mfpu=neon-fp16
CFLAGS+=-march=native

CFLAGS+=-DLOG2FFTSIZE=12
CFLAGS+=-DFFT_REPEAT=1000

# improve speed
CFLAGS+=-DMOD_SPEED

LDFLAGS=-lm -s
OBJS=fft.o benchmark.o

TARGET=benchmark

all: $(OBJS)
	$(CC) $(OBJS) $(LDFLAGS) -o $(TARGET)

clean:
	rm -rf $(OBJS) $(TARGET)
