#!/usr/bin/pypy
# -*- coding: utf8 -*-

from __future__ import print_function

import fft
import time
import multiprocessing

PROCESSNUM = 4
LOG2FFTSIZE = 12
FFT_REPEAT = 1000

SIZE = 1<<LOG2FFTSIZE
xy = [0+0j]*SIZE

def fft_main(args):
    xy, loopnum = args
    fft_out = []
    for i in range(loopnum):
        fft_out = fft.fft(LOG2FFTSIZE, xy)
    return fft_out

if __name__ == "__main__":
    xy = [1.+0j]*(SIZE//2) + [-1.+0j]*(SIZE//2)

    timestart = time.time()

    pool = multiprocessing.Pool(processes=PROCESSNUM)
    arg=(xy, FFT_REPEAT//PROCESSNUM)
    fft_out_list = pool.map(fft_main, [arg]*PROCESSNUM)

    eltime = 1000*(time.time() - timestart)
    print ("%6d piece of %d pt FFT;  %9.5f ms/piece\n"%(FFT_REPEAT, 1<<LOG2FFTSIZE, eltime/FFT_REPEAT))

    for i in range(6):
        print (i, fft_out_list[0][i], fft_out_list[1][i], fft_out_list[2][i], fft_out_list[3][i])
