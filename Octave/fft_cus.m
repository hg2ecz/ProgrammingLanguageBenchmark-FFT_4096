function xy_out = fft_cus(log2point, xy_in)
    fftpoint = bitshift(1, log2point);
    xy_out = zeros(1, fftpoint);
    for i = 0 : fftpoint-1
	brev = 0;
	bit = 1;
	bitr = fftpoint/2;
	for x=1 : log2point
	    if bitand(i, bit)
		brev+=bitr;
	    endif
	    bit *=2;
	    bitr/=2;
	endfor
	xy_out(brev+1) = xy_in(i+1);
    endfor

    # here begins the Danielson-Lanczos section
    n = fftpoint;
    mmax=1;
    while n > mmax
	istep = mmax*2;

	theta = -2*pi/istep;
	wphase_XY = cos(theta) + sin(theta)*j;

	w_XY = 1. + 0.*j;
	for m = 1 : mmax
	    for i = m : istep : n
		tempXY = w_XY * xy_out(i+mmax);
		xy_out(i+mmax)  = xy_out(i) - tempXY;
		xy_out(i     ) += tempXY;
	    endfor
	    w_XY *= wphase_XY; # rotate
	endfor
	mmax=istep;
    endwhile
end
