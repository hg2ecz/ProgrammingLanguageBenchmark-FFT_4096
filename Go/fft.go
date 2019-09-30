package main

import "math"

func initializePhaseVector() *[32]complex128 {
	var phaseVect [32]complex128

	for i := 0; i < 32; i++ {
		num := 2
		point := float64(num << uint(i))
		phaseVect[i] = complex(math.Cos(-2*math.Pi/point), math.Sin(-2*math.Pi/point))
	}

	return &phaseVect
}

// Fft Public function
func Fft(log2point uint, xyIn *[4096]complex128, phaseVect *[32]complex128) *[4096]complex128 {
	var xyOut [4096]complex128

	for i := 0; i < (1 << log2point); i++ {
		var brev = uint(i)
		brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1)
		brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2)
		brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4)
		brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8)
		brev = (brev >> 16) | (brev << 16)

		brev >>= 32 - log2point
		xyOut[brev] = xyIn[i]
	}

	// here begins the Danielson-Lanczos section
	n := 1 << log2point
	l2pt := 0
	mmax := 1

	for n > mmax {
		istep := mmax << 1
		//	double theta = -2*M_PI/istep
		//	double complex wphase_XY = cos(theta) + sin(theta)*I
		wphaseXY := phaseVect[l2pt]
		l2pt++

		wXY := complex(1.0, 0.0)

		for m := 0; m < mmax; m++ {
			for i := m; i < n; i += istep {
				tempXY := wXY * xyOut[i+mmax]
				xyOut[i+mmax] = xyOut[i] - tempXY
				xyOut[i] += tempXY
			}

			wXY *= wphaseXY
		}

		mmax = istep
	}

	return &xyOut
}
