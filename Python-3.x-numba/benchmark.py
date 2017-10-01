#!/usr/bin/python3
# -*- coding: utf8 -*-

from __future__ import print_function

import fft
import time

LOG2FFTSIZE = 12
FFT_REPEAT = 1000

SIZE = 1<<LOG2FFTSIZE
xy = [0+0j]*SIZE

if __name__ == "__main__":
    xy = [1.+0.j]*(SIZE//2) + [-1.+0.j]*(SIZE//2)

    timestart = time.time()
    for i in range(FFT_REPEAT):
        fft_out = fft.fft(LOG2FFTSIZE, xy)
    eltime = 1000*(time.time() - timestart)
    print ("%6d piece of %d pt FFT;  %9.5f ms/piece\n"%(FFT_REPEAT, 1<<LOG2FFTSIZE, eltime/FFT_REPEAT))

    for i in range(6):
        print (i, fft_out[i])
