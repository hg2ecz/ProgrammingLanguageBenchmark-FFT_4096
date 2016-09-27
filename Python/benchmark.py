#!/usr/bin/pypy
# -*- coding: utf8 -*-

import fft
import time

LOG2FFTSIZE = 16
FFT_REPEAT = 10

SIZE = 1<<LOG2FFTSIZE
xy = [0+0j]*SIZE

if __name__ == "__main__":

    for i in xrange(SIZE/2):
	xy[i]= 1.;

    for i in xrange(SIZE/2, SIZE):
	xy[i]=-1.;

    timestart = time.time()
    for i in xrange(FFT_REPEAT):
	fft_out = fft.fft(16, xy)
    eltime = 1000*(time.time() - timestart);
    print "%6d piece of %d pt FFT;  %9.5f ms/piece\n"%(FFT_REPEAT, 1<<LOG2FFTSIZE, eltime/FFT_REPEAT)

    for i in xrange(6):
	print i, fft_out[i]
