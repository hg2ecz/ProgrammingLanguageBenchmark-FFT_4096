#!/usr/bin/ruby

require "./fft"

LOG2FFTSIZE = 12
FFT_REPEAT = 100

SIZE = (1<<LOG2FFTSIZE)
$xy = Array.new(SIZE)
$xy_out_fft = Array.new(SIZE)

def main()
    
    for i in 0 .. SIZE/2-1
	$xy[i] = 1.+0i
    end
    for i in SIZE/2 .. SIZE-1
	$xy[i] = -1.+0i
    end
# FFT
    startTime = Time.now
    for i in 0 .. FFT_REPEAT-1
	fft(LOG2FFTSIZE, $xy_out_fft, $xy)
    end
    endTime = Time.now

    printf("%8d piece(s) of %d pt FFT;    %.3f ms/piece\n\n", FFT_REPEAT, SIZE, (endTime-startTime)*1000.0/FFT_REPEAT)

    for i in 0 .. 6-1
	printf("%d\t%f\t%f\n", i, $xy_out_fft[i].real, $xy_out_fft[i].imag)
    end
end

main()
