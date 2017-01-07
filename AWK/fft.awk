# Internal variables
phasevec_exist = 0;

# Public function
function fft(log2point, xy_in, xy_out) {
    if (phasevec_exist == 0) {
	pi = atan2(0, -1)
	for (i=0; i<32; i++) {
	    phpoint = lshift(1, i+1)
	    phasevec[i][0] = cos(-2*pi/phpoint)
	    phasevec[i][1] = sin(-2*pi/phpoint)
	}
	phasevec_exist = 1
    }

    point=lshift(1, log2point)

    for (i=0; i<point; i++) {
	brev = i
	brev = or(rshift(and(brev, 0xaaaaaaaa), 1), lshift(and(brev, 0x55555555), 1))
	brev = or(rshift(and(brev, 0xcccccccc), 2), lshift(and(brev, 0x33333333), 2))
	brev = or(rshift(and(brev, 0xf0f0f0f0), 4), lshift(and(brev, 0x0f0f0f0f), 4))
	brev = or(rshift(and(brev, 0xff00ff00), 8), lshift(and(brev, 0x00ff00ff), 8))
	brev = or(rshift(brev, 16), lshift(brev, 16))

	brev = rshift(brev, 32-log2point)
	xy_out[brev][0] = xy_in[i][0]
	xy_out[brev][1] = xy_in[i][1]
    }

# here begins the Danielson-Lanczos section
    n = lshift(1, log2point)
    l2pt=0
    mmax=1
    while (n > mmax) {
	istep = lshift(mmax, 1)
#	double theta = -2*M_PI/istep;
#	double complex wphase_XY = cos(theta) + sin(theta)*I;
	wphase_XY[0] = phasevec[l2pt][0]
	wphase_XY[1] = phasevec[l2pt][1]
	l2pt++

	w_XY[0] = 1.0
        w_XY[1] = 0.0
	for (m=0; m<mmax; m++) {
	    for (i=m; i<n; i+=istep) {
		tempXY[0] = w_XY[0] * xy_out[i+mmax][0] - w_XY[1] * xy_out[i+mmax][1]
		tempXY[1] = w_XY[0] * xy_out[i+mmax][1] + w_XY[1] * xy_out[i+mmax][0]

		xy_out[i+mmax][0]  = xy_out[i][0] - tempXY[0]
		xy_out[i+mmax][1]  = xy_out[i][1] - tempXY[1]

		xy_out[i     ][0] = xy_out[i     ][0] + tempXY[0]
		xy_out[i     ][1] = xy_out[i     ][1] + tempXY[1]
	    }
	    w_XY_t  = w_XY[0] * wphase_XY[0] - w_XY[1] * wphase_XY[1]
	    w_XY[1] = w_XY[0] * wphase_XY[1] + w_XY[1] * wphase_XY[0]
	    w_XY[0] = w_XY_t;
	}
	mmax=istep;
    }
#    return xy_out; -- see func 2. value
}
