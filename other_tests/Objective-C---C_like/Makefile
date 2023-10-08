OBJC=gcc
CPPFLAGS =-Wall -Ofast -funroll-all-loops
CPPFLAGS+= `gnustep-config --objc-flags`

PARAMS_CPU = -march=native
ifeq ($(shell uname -m), armv7l)
    PARAMS_CPU = -mcpu=cortex-a5 -mfpu=neon-fp16 
endif
CPPFLAGS+= $(PARAMS_CPU)

CPPFLAGS+=-DLOG2FFTSIZE=12
CPPFLAGS+=-DFFT_REPEAT=1000

# improve speed
CPPFLAGS+=-DMOD_SPEED

LDFLAGS=-lm -s
LDFLAGS+=`gnustep-config --base-libs`
OBJS=fft.o benchmark.o

TARGET=benchmark



all: $(OBJS)
	$(OBJC) $(OBJS) $(LDFLAGS) -o $(TARGET)

clean:
	rm -rf $(OBJS) $(OBJS:.o=.d) $(TARGET)

PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py
