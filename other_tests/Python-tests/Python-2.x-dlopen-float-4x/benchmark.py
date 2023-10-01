#!/usr/bin/python
# -*- coding: utf8 -*-

from __future__ import print_function

import time
import ctypes

LOG2FFTSIZE = 12
FFT_REPEAT = 1000

SIZE = 1<<LOG2FFTSIZE
complexArrayType = (2 * ctypes.c_float) * SIZE

if __name__ == "__main__":
    fft = ctypes.CDLL("../C-fast-all-complex-float-types-opt-4x/fft.so")
    cxy = complexArrayType()
    fft_out = complexArrayType()
    for i in xrange(SIZE/2):
        cxy[i][0] = 1.
        cxy[i][1] = 0.
    for i in xrange(SIZE/2, SIZE):
        cxy[i][0] = -1.
        cxy[i][1] = 0.

    timestart = time.time()
    for i in xrange(FFT_REPEAT):
        fft.fft(LOG2FFTSIZE, fft_out, cxy)
    eltime = 1000*(time.time() - timestart)
    print ("%6d piece of %d pt FFT;  %9.5f ms/piece\n"%(FFT_REPEAT, 1<<LOG2FFTSIZE, eltime/FFT_REPEAT))

    for i in xrange(6):
        print ("%3d %16.4f %16.4f"%(i, fft_out[i][0], fft_out[i][1]))
