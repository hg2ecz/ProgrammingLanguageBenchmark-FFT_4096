# -*- coding: utf8 -*-

import math
from numba import jit

# Public function
@jit
def fft(log2point, xy_in):
    xy_out = [0+0j]*(1<<log2point)
    for i in xrange(1<<log2point):
        brev = i
        brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1)
        brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2)
        brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4)
        brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8)
        brev = (brev >> 16) | (brev << 16)

        brev >>= 32-log2point
        xy_out[brev] = xy_in[i]


    # here begins the Danielson-Lanczos section
    n = 1<<log2point
    l2pt=0
    mmax=1
    while (n>mmax):
        istep = mmax<<1

        wphase_XY = math.cos(-2*math.pi/(2<<l2pt)) + math.sin(-2*math.pi/(2<<l2pt))*1j

        l2pt+=1

        w_XY = 1.0 + 0.0*1j
        for m in xrange(mmax):
            for i in xrange(m, n, istep):
                tempXY = w_XY *xy_out[i+mmax]
                xy_out[i+mmax]  = xy_out[i] - tempXY
                xy_out[i     ] += tempXY
            w_XY *= wphase_XY; # rotate
        mmax=istep

    return xy_out
