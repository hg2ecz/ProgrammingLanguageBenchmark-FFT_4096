# -*- coding: utf8 -*-

import math

# Internal variables
phasevec_exist = 0;
phasevec = [0. + 0j]*32;

# Public function
def fft(log2point, xy_in):
    global phasevec_exist;
    if not phasevec_exist:
        for i in range(32):
            point = 2<<i;
            phasevec[i] = math.cos(-2*math.pi/point) + math.sin(-2*math.pi/point)*1j;
        phasevec_exist = 1;

    xy_out = [0+0j]*(1<<log2point);
    for i in range(1<<log2point):
        brev = i;
        brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1);
        brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2);
        brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4);
        brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8);
        brev = (brev >> 16) | (brev << 16);

        brev >>= 32-log2point;
        xy_out[brev] = xy_in[i];


    # here begins the Danielson-Lanczos section
    n = 1<<log2point;
    l2pt=0;
    mmax=1;
    while (n>mmax):
        istep = mmax<<1;

        wphase_XY = phasevec[l2pt];
        l2pt+=1;

        w_XY = 1.0 + 0.0*1j;
        for m in range(mmax):
            for i in range(m, n, istep):
                tempXY = w_XY *xy_out[i+mmax];
                xy_out[i+mmax]  = xy_out[i] - tempXY;
                xy_out[i     ] += tempXY;
            w_XY *= wphase_XY; # rotate
        mmax=istep;

    return xy_out
