CC=gcc
CFLAGS =-Wall -O2 -fopenmp

PARAMS_CPU = -march=native
ifeq ($(shell uname -m), armv7l)
    PARAMS_CPU = -mcpu=cortex-a5 -mfpu=neon-fp16 
endif
CFLAGS+= $(PARAMS_CPU)

CFLAGS+=-DLOG2FFTSIZE=12
CFLAGS+=-DFFT_REPEAT=1000

# improve speed
CFLAGS+=-DMOD_SPEED

LDFLAGS=-lm -s -lfftw3f -lgomp
OBJS=benchmark.o

TARGET=benchmark



all: $(OBJS)
	$(CC) $(OBJS) $(LDFLAGS) -o $(TARGET)

clean:
	rm -rf $(OBJS) $(TARGET)

PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py
