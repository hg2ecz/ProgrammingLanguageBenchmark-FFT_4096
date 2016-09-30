CC=gcc

CFLAGS =-Wall -Ofast -funroll-all-loops
CFLAGS+=-mcpu=cortex-a5 -mfpu=neon-fp16

CFLAGS+=-DLOG2FFTSIZE=16
CFLAGS+=-DFFT_REPEAT=10

# improve speed
CFLAGS+=-DMOD_SPEED

LDFLAGS=-lm -s
OBJS=fft.o benchmark.o

TARGET=benchmark

all: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o $(TARGET)

clean:
	rm -rf $(OBJS) $(TARGET)
