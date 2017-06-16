#!/usr/bin/python
# -*- coding: utf8 -*-

import numpy

LOG2FFTSIZE = 12
SIZE = 1<<LOG2FFTSIZE

print "--- FFT: ---"
sample = [1. + 0j]*(SIZE/2) + [-1.+ 0j]*(SIZE/2)
res = numpy.fft.fft(sample)
for i in range(6):
    print res[i]

print "\n--- iFFT: ---"
res2 = numpy.fft.ifft(res)
for i in range(6):
    print res2[i]
