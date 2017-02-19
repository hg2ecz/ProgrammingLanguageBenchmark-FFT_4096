# Internal variables
$phasevec_exist = false
$phasevec = Array.new(32)

# Public function
def fft(log2point, xy_out, xy_in)
    if !$phasevec_exist
	for i in 0 .. 32-1
	    point = 2<<i
	    $phasevec[i] = Complex(Math.cos(-2.0*Math::PI/point), Math.sin(-2.0*Math::PI/point))
	end
	$phasevec_exist = true
    end

    for i in 0 .. (1<<log2point)-1
	brev = i
	brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1)
	brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2)
	brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4)
	brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8)
	brev = (brev >> 16) | (brev << 16)

	brev >>= 32-log2point
	xy_out[brev] = xy_in[i]
    end

    # here begins the Danielson-Lanczos section
    n = 1<<log2point
    l2pt = 0
    mmax = 1

    while n > mmax
	istep = mmax<<1
#	double theta = -2*M_PI/istep
#	double complex wphase_XY = cos(theta) + sin(theta)*I
	wphase_XY = $phasevec[l2pt]
	l2pt+=1

	w_XY = 1.0+0.0i
	for m in 0 .. mmax-1
	    for i in (m .. n-1).step(istep)
		tempXY = w_XY * xy_out[i+mmax]
		xy_out[i+mmax]  = xy_out[i] - tempXY
		xy_out[i     ] += tempXY
	    end
	    w_XY *= wphase_XY; # rotate
	end
	mmax=istep
    end
end
