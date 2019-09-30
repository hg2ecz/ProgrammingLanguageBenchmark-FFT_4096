package main

import (
	"fmt"
	"time"
)

// Log2OffsetSize is the logarithm of number of points.
const Log2OffsetSize uint = 12

// FftRepeat Number is iterations for measurement.
const FftRepeat int = 1000

// Size is the number of points.
const Size = (1 << Log2OffsetSize)

func initializeInputArray() *[Size]complex128 {
	var xy [Size]complex128
	var i int

	for i = 0; i < Size/2; i++ {
		xy[i] = (1. + 0i)
	}
	for ; i < Size; i++ {
		xy[i] = (-1. + 0i)
	}

	return &xy
}

func main() {
	xy := initializeInputArray()
	startTime := time.Now().UnixNano()
	phaseVect := initializePhaseVector()

	for j := 0; j < FftRepeat; j++ {
		xyOutFft := Fft(Log2OffsetSize, xy, phaseVect)

		if j == FftRepeat-1 {
			var endTime = time.Now().UnixNano()

			fmt.Printf("%8d piece(s) of %d pt FFT;    %.3f ms/piece\n\n", FftRepeat, Size, float64(endTime-startTime)/float64(1e6)/float64(FftRepeat))

			for i := 0; i < 6; i++ {
				fmt.Printf("%d %f %f\n", i, real(xyOutFft[i]), imag(xyOutFft[i]))
			}
		}
	}
}
