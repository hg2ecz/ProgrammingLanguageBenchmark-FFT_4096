#!/usr/bin/gawk -f

# Caution: all variables are global in AWK, do not use same variable name
@include "fft"

BEGIN {
    LOG2FFTSIZE = 12
    FFT_REPEAT = 10

    SIZE = 2 ** LOG2FFTSIZE

    for (i=0; i<SIZE/2; i++) {
        fft_in[i][0] = 1.
        fft_in[i][1] = 0
    }

    for (  ; i<SIZE; i++) {
        fft_in[i][0] = -1.
        fft_in[i][1] = 0
    }

    uptime = "/proc/uptime"
    getline < uptime; close(uptime)
    timestart = $1

    for (fft_rep=0; fft_rep<FFT_REPEAT; fft_rep++) {
        fft(LOG2FFTSIZE, fft_in, fft_out)
    }

    getline < uptime; close(uptime)
    timeend = $1

    eltime = 1000*(timeend - timestart)
    print (FFT_REPEAT, " piece of ", SIZE, " pt FFT; ", eltime/FFT_REPEAT, " ms/piece\n")

    for (i=1; i<7; i++) {
        print (i, fft_out[i][0], fft_out[i][1])
    }
    exit(0)
}
