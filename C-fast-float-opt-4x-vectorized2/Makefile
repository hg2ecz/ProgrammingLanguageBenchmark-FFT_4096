include Makefile-opt

CFLAGS+=-DLOG2FFTSIZE=12
CFLAGS+=-DFFT_REPEAT=100000
# check the result
CFLAGS+=-DCHECKFFT


OBJS=fft_vec.o fft_cplx.o dft_test.o benchmark.o

TARGET=benchmark

all:
	make fft_const_h
	make build

PHASEVECGEN = fft_phasevec_calc
fft_const_h:
	$(CC) $(CFLAGS) $(PHASEVECGEN).c $(LDFLAGS) -o $(PHASEVECGEN)
	./$(PHASEVECGEN) > fft_const.h

build: $(OBJS)
	$(CC) $(OBJS) $(LDFLAGS) -o $(TARGET)

clean:
	rm -rf $(OBJS) $(TARGET) $(PHASEVECGEN) fft_const.h
