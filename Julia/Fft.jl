module Fft
    function fft_init()
	phasevec = repeat([0.0 + 0.0im], 32)
	for i=1:32
	    point = 1<<i
	    phasevec[i] = cos(-2*pi/point) + sin(-2*pi/point)*1.0im
	end
	return phasevec
    end

    function fft(log2point, xy_in, phasevec)
	size = 1<<log2point
	xy_out = repeat([0.0 + 0.0im], size)
	for i=1:size
	    brev = convert(UInt32, i-1)
	    brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1)
	    brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2)
	    brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4)
	    brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8)
	    brev = (brev >> 16) | (brev << 16)

	    brev >>= (32-log2point)
	    xy_out[brev+1] = xy_in[i]
	end

	# here begins the Danielson-Lanczos section
	n = size
	l2pt=1
	mmax=1

	while n>mmax
	    istep = mmax<<1

	    #theta = -2*pi/istep;
	    #wphase_XY = cos(theta) + sin(theta)*1.0im;
	    wphase_XY = phasevec[l2pt]
	    l2pt+=1

	    w_XY = 1.0 + 0.0im
	    for m=1:mmax
		for i=m:istep:n
		    tempXY = w_XY *xy_out[i+mmax]
		    xy_out[i+mmax]  = xy_out[i] - tempXY
		    xy_out[i     ] += tempXY
		end
		w_XY *= wphase_XY   # rotate
	    end
	    mmax=istep
	end
        return xy_out
    end
end
