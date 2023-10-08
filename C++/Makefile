CXX=g++
CXXFLAGS =-Wall -Ofast -funroll-all-loops -flto

PARAMS_CPU = -march=native
ifeq ($(shell uname -m), armv7l)
    PARAMS_CPU = -mcpu=cortex-a5 -mfpu=neon-fp16 
endif
CXXFLAGS+= $(PARAMS_CPU)

CXXFLAGS+=-DLOG2FFTSIZE=12
CXXFLAGS+=-DFFT_REPEAT=1000

# improve speed
CXXFLAGS+=-DMOD_SPEED

LDFLAGS=-lm -s -flto
OBJS=fft.o benchmark.o

TARGET=benchmark



all: $(OBJS)
	$(CXX) $(OBJS) $(LDFLAGS) -o $(TARGET)

clean:
	rm -rf $(OBJS) $(TARGET)

PROG=$(TARGET)
test:
	./$(PROG)
	@(./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG);  ./$(PROG);./$(PROG);./$(PROG);./$(PROG);./$(PROG) )|../avgcalc.py
