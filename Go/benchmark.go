package main

import (
	"fmt"
	"time"
	"math"
	"fft"
)

// Log2OffsetSize is the logarithm of number of points.
const Log2OffsetSize uint = 12

// FftRepeat Number is iterations for measurement.
const FftRepeat int = 1000

// Size is the number of points.
const Size = (1 << Log2OffsetSize)

func initializeInputArray() []complex64 {
	xy := make([]complex64, Size)
	var i int
	for i = 0; i < Size/2; i++ {
		xy[i] = complex(float32(1.), 0.)
	}
	for ; i < Size; i++ {
		xy[i] = complex(float32(-1.), 0.)
	}
	return xy
}

func main() {
	xy := initializeInputArray()
	phaseVect := fft.InitializePhaseVector()
	xyOutFft :=  make([]complex64, Size)
	startTime := time.Now().UnixNano()
	for j := 0; j < FftRepeat; j++ {
		fft.Fft(Log2OffsetSize, xyOutFft, xy, phaseVect)
	}
	var endTime = time.Now().UnixNano()
	fmt.Printf("%8d piece(s) of %d pt FFT;    %.3f ms/piece\n\n", FftRepeat, Size, float64(endTime-startTime)/float64(1e6)/float64(FftRepeat))
	fmt.Printf("bin        real             imag           absval\n")
	for i := 0; i < 6; i++ {
		absval := math.Sqrt(float64(real(xyOutFft[i]) * real(xyOutFft[i]) + imag(xyOutFft[i]) * imag(xyOutFft[i])))
		fmt.Printf("%3d %16.4f %16.4f %16.4f\n", i, real(xyOutFft[i]), imag(xyOutFft[i]), absval)
	}
}
