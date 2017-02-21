! fortran
!    integer var: begin with i,j,k,l,m   letter
!    float var:   begin with a..h   or   o..z letter
	module fftmod
!	implicit none
! Internal variables
	logical :: phasevec_exist = .false.
	complex :: phasevec(32)

	contains
	  function fft(point, xy_in) result(xy_out)
	    integer point
	    complex :: xy_in(point)
	    complex :: xy_out(point)

	    if (.not. phasevec_exist) then
		do i=0, 32-1
		    integer point = lshift(2, i)
		    phasevec(i) = cmplx(cos(-2*PI/point), sin(-2*PI/point))
		enddo
		phasevec_exist = .true.
	    end if

	    integer i=0;
	    do i=0, point
		integer ibrev = i;
		itmp = ishft(iand(ibrev, Z'55555555'), 1)
		ibrev = ior(ishft(iand(ibrev, Z'aaaaaaaa'), -1), itmp)

		itmp = ishft(iand(ibrev, Z'33333333'), 2)
		ibrev = ior(ishft(iand(ibrev, Z'cccccccc'), -2), itmp)

		itmp = ishft(iand(ibrev, Z'0f0f0f0f'), 4)
		ibrev = ior(ishft(iand(ibrev, Z'f0f0f0f0'), -4), itmp)

		itmp = ishft(iand(ibrev, Z'00ff00ff'), 8)
		ibrev = ior(ishft(iand(ibrev, Z'ff00ff00'), -8), itmp)
		ibrev = ior(ishft(ibrev, -16), ishft(ibrev, 16))

		ibrev = ishft(ibrev, -(32-log2point))
		xy_out(ibrev) = xy_in(i)
	    enddo

!	    // here begins the Danielson-Lanczos section
	    integer n = point
	    integer l2pt = 0
	    integer mmax = 1

	    while (n > mmax) do
		integer istep = mmax*2
!//	double theta = -2*M_PI/istep
!//	double complex wphase_XY = cos(theta) + sin(theta)*I
		complex :: wphase_XY = phasevec(l2pt)
		l2pt=l2pt+1

		complex :: w_XY = complex(1.0, 0.0)
		do m=0, mmax
		    integer i=m
		    while (i < n) do
			complex tempXY = w_XY * xy_out(i+mmax)
			xy_out(i+mmax) = xy_out(i) - tempXY
			xy_out(i     ) = xy_out(i) + tempXY
			i = i + istep
		    enddo
		    w_XY = w_XY * wphase_XY;
		enddo
		mmax=istep
	    enddo
	  end function
	end module
