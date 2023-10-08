#!/usr/bin/python3

import numpy

LOG2FFTSIZE = 12
SIZE = 1<<LOG2FFTSIZE

print ("--- FFT: ---")
sample = [1. + 0j]*(SIZE//2) + [-1.+ 0j]*(SIZE//2)
res = numpy.fft.fft(sample)
print("bin        real             imag           absval");
for i in range(6):
    print ("%3d %16.4f %16.4f %16.4f"%(i, res[i].real, res[i].imag, abs(res[i])))


print ("\n--- iFFT: ---")
res2 = numpy.fft.ifft(res)
print("bin        real             imag           absval");
for i in range(6):
    print ("%3d %16.4f %16.4f %16.4f"%(i, res2[i].real, res2[i].imag, abs(res2[i])))
