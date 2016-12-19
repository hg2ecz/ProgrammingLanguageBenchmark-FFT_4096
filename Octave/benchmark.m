#!/usr/bin/octave

LOG2FFTSIZE = 12
FFT_REPEAT = 10

SIZE = bitshift(1, LOG2FFTSIZE);
xy = zeros(1, SIZE);

function main(log2fftsize, xy, fft_repeat)
    fftsize = bitshift(1, log2fftsize);

    for i = 1 : fftsize/2
	xy(i) = 1. + 0*j;
    endfor

    for i = fftsize/2+1 : fftsize
	xy(i) = -1. + 0*j;
    endfor

    timestart = clock();
    for i = 0 : fft_repeat
	fft_out = fft_cus(log2fftsize, xy); # fft is a valid Octave function
    endfor
    eltime = 1000.*( etime (clock (), timestart) );
    printf ("%6d piece of %d pt FFT;  %9.5f ms/piece\n", fft_repeat, fftsize, eltime/fft_repeat);
    for i = 1 : 6
	printf("%3d %16.9f %16.9f\n", i, real(fft_out(i)), imag(fft_out(i)));
    endfor
end

main(LOG2FFTSIZE, xy, FFT_REPEAT)
