package main

import "math"

// Internal variables
var phasevecExist = false
var phasevec [32]complex128

// Public function
func Fft(log2point uint, xyIn *[4096]complex128) *[4096]complex128 {
	var xyOut [4096]complex128

	if !phasevecExist {
		for i := 0; i < 32; i++ {
			var num uint = 2
			var point float64 = float64(num << uint(i))
			phasevec[i] = complex(math.Cos(-2*math.Pi/point), math.Sin(-2*math.Pi/point))
		}
		phasevecExist = true
	}

	for i := 0; i < (1 << log2point); i++ {
		var brev uint = uint(i)
		brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1)
		brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2)
		brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4)
		brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8)
		brev = (brev >> 16) | (brev << 16)

		brev >>= 32 - log2point
		xyOut[brev] = xyIn[i]
	}

	// here begins the Danielson-Lanczos section
	var n int = 1 << log2point
	var l2pt int = 0
	var mmax int = 1

	for n > mmax {
		var istep int = mmax << 1
		//	double theta = -2*M_PI/istep
		//	double complex wphase_XY = cos(theta) + sin(theta)*I
		var wphase_XY complex128 = phasevec[l2pt]
		l2pt++

		var w_XY complex128 = complex(1.0, 0.0)
		for m := 0; m < mmax; m++ {
			for i := m; i < n; i += istep {
				var tempXY complex128 = w_XY * xyOut[i+mmax]
				xyOut[i+mmax] = xyOut[i] - tempXY
				xyOut[i] += tempXY
			}
			w_XY *= wphase_XY // rotate
		}
		mmax = istep
	}
	return &xyOut
}
