package main

import (
    "fmt"
    "time"
    "dft"
)

const LOG2FFTSIZE uint= 12
const FFT_REPEAT int = 10

const SIZE int = (1<<LOG2FFTSIZE)
var xy [SIZE]complex128
var xy_out_fft [SIZE]complex128

func main() {
    var i int
    for i=0; i<SIZE/2; i++ { xy[i]= (1.+0i) }
    for    ; i<SIZE  ; i++ { xy[i]= (-1.+0i) }

// FFT
    dft.Dft_init(SIZE)
    var startTime = time.Now().UnixNano()
    for i=0; i<FFT_REPEAT; i++ {
	for j:=0; j<SIZE; j++ {
	    dft.Dft_sample(&xy_out_fft, xy[j])
	}
    }
    var endTime = time.Now().UnixNano()

    fmt.Printf("%8d piece(s) of %d pt FFT;    %.3f ms/piece\n\n", FFT_REPEAT, SIZE, float64(endTime-startTime)/float64(1e6)/float64(FFT_REPEAT))

    for i=0; i<6; i++ {
	fmt.Println(i, xy_out_fft[i])
    }
}
