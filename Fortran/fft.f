! fortran
!    integer var: begin with i,j,k,l,m   letter
!    float var:   begin with a..h   or   o..z letter
	module fftmod
!	implicit none

! Internal variables
	logical :: phasevec_exist = .false.
	complex*16 :: phasevec(32)

	real*8 PI

	complex*16 :: wphase_XY
	complex*16 :: w_XY
	complex*16 :: tempXY

	contains
	  function fft(point, xy_out, xy_in) result(i)
	    integer :: point
	    complex*16 :: xy_out(point)
	    complex*16 :: xy_in(point)

	    integer :: i

	    if (.not. phasevec_exist) then
		PI=4.D0*DATAN(1.D0)
		do i=1, 32-1
		    pt = ishft(1, i)
		    phasevec(i) = complex(cos(-2.D0*PI/pt), sin(-2.D0*PI/pt))
		enddo
		phasevec_exist = .true.
	    end if

	    do i=1, point
		ibrev = i-1;
		itmp = ishft(iand(ibrev, Z'55555555'), 1)
		ibrev = ior(ishft(iand(ibrev, Z'aaaaaaaa'), -1), itmp)

		itmp = ishft(iand(ibrev, Z'33333333'), 2)
		ibrev = ior(ishft(iand(ibrev, Z'cccccccc'), -2), itmp)

		itmp = ishft(iand(ibrev, Z'0f0f0f0f'), 4)
		ibrev = ior(ishft(iand(ibrev, Z'f0f0f0f0'), -4), itmp)

		itmp = ishft(iand(ibrev, Z'00ff00ff'), 8)
		ibrev = ior(ishft(iand(ibrev, Z'ff00ff00'), -8), itmp)
		ibrev = ior(ishft(ibrev, -16), ishft(ibrev, 16))

		ibrev = ishft(ibrev, -(32-12))
		xy_out(ibrev+1) = xy_in(i)
	    enddo

!	    // here begins the Danielson-Lanczos section
	    n = point
	    l2pt = 1
	    mmax = 1
	    do while (n > mmax)
		istep = ishft(mmax, 1)
!//	double theta = -2*M_PI/istep
!//	double complex wphase_XY = cos(theta) + sin(theta)*I
		wphase_XY = phasevec(l2pt)
		l2pt = l2pt+1
		w_XY = complex(1.0, 0.0)
		do m=1, mmax
		    do i=m, n, istep
			tempXY = w_XY * xy_out(i+mmax)
			xy_out(i+mmax) = xy_out(i) - tempXY
			xy_out(i     ) = xy_out(i) + tempXY
		    enddo
		    w_XY = w_XY * wphase_XY
		enddo
		mmax=istep
	    enddo
	    i=0
	  end function
	end module
