package dft

import "math"
import "math/cmplx"

const LOG2FFTSIZE uint= 12
const SIZE = (1<<LOG2FFTSIZE)

var dft_point int = 0
var dft_revpt complex128

var dft_phasediffXY complex128
var dft_vfoXY[1<<LOG2FFTSIZE] complex128


func Dft_init(point int) {
    dft_point = point
    dft_revpt = complex(1./float64(point), 0)
    //phasediffXY = cos(-2*M_PI/point) + I*sin(-2*M_PI/point);
    dft_phasediffXY = cmplx.Exp(-2i*math.Pi*dft_revpt)

    for i:=0; i<point; i++ {
	dft_vfoXY[i]=1.
    }
}

var dft_corr complex128 = 0
func Dft_sample(xy_out[SIZE] complex128, xy_sample complex128) [SIZE]complex128 {
    xy_sample -= dft_corr*dft_revpt;		// remove oldest value from DFT

    dft_corr = 0
    var phaseXY complex128 = 1.

/*  ... slower
    for i:=0; i<dft_point; i++ {
	xy_out[i] += xy_sample*dft_vfoXY[i]	// new DFT value

	dft_vfoXY[i] *= phaseXY			// oscillator new state
	phaseXY *= dft_phasediffXY		// oscillator new speed
	dft_corr += xy_out[i]*cmplx.Conj(dft_vfoXY[i])	// generate new prev. correction
    }
*/
    for i:=0; i<dft_point; i++ {
	var tmp_vfoXY complex128 = dft_vfoXY[i] // register --> faster
	var tmp_xy_out complex128 = xy_out[i]	// register --> faster

	tmp_xy_out += xy_sample*tmp_vfoXY	// new DFT value

	tmp_vfoXY *= phaseXY			// oscillator new state
	phaseXY *= dft_phasediffXY		// oscillator new speed
	dft_corr += tmp_xy_out*cmplx.Conj(tmp_vfoXY)	// generate new prev. correction
	dft_vfoXY[i]=tmp_vfoXY
	xy_out[i] = tmp_xy_out
    }

    return xy_out
}
