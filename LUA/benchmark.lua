#!/usr/bin/luajit

require "bit"
require "fft"

local LOG2FFTSIZE = 12
local FFT_REPEAT = 1000

local SIZE = bit.lshift(1, LOG2FFTSIZE)
local xy = {}

function main(LOG2FFTSIZE, xy, FFT_REPEAT)
    for i=1, SIZE/2 do
	xy[i]= {1., 0};
    end

    for i=SIZE/2+1, SIZE do
	xy[i]={ -1., 0 };
    end

    timestart = os.clock()
    for i=1, FFT_REPEAT do
	fft_out = fft(LOG2FFTSIZE, xy)
    end
    eltime = 1000*(os.clock() - timestart);
    print (string.format("%6d piece of %d pt FFT;  %9.5f ms/piece\n", FFT_REPEAT, SIZE, eltime/FFT_REPEAT))

    for i=1, 7 do
	print (i, fft_out[i][1], fft_out[i][2])
    end
end

main(LOG2FFTSIZE, xy, FFT_REPEAT)
