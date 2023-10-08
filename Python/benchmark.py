#!/usr/bin/pypy3

import fft
import time

LOG2FFTSIZE = 12
FFT_REPEAT = 1000

SIZE = 1<<LOG2FFTSIZE
xy = [0+0j]*SIZE

if __name__ == "__main__":
    xy = [1.+0j]*(SIZE//2) + [-1.+0j]*(SIZE//2)

    timestart = time.time()
    for i in range(FFT_REPEAT):
        fft_out = fft.fft(LOG2FFTSIZE, xy)
    eltime = 1000*(time.time() - timestart)
    print ("%6d piece of %d pt FFT;  %9.5f ms/piece\n"%(FFT_REPEAT, 1<<LOG2FFTSIZE, eltime/FFT_REPEAT))

    print("bin        real             imag           absval");
    for i in range(6):
        print ("%3d %16.4f %16.4f %16.4f"%(i, fft_out[i].real, fft_out[i].imag, abs(fft_out[i])))
