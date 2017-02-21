! fortran
!    integer var: begin with i,j,k,l,m   letter
!    float var:   begin with a..h   or   o..z letter
	module fftmod
!	implicit none
! Internal variables
	logical :: phasevec_exist = .false.
	complex :: phasevec(32)

	contains
	  function fft(ipoint, xy_in) result(xy_out)
	    integer :: ipoint
	    complex :: xy_in(ipoint)
	    complex :: xy_out(ipoint)

	    if (.not. phasevec_exist) then
		do i=0, 32-1
		    integer pt = ishft(2, i)
		    phasevec(i) = cmplx(cos(-2*PI/pt), sin(-2*PI/pt))
		enddo
		phasevec_exist = .true.
	    end if

	    integer i=0;
	    do i=0, ipoint
		ibrev = i;
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
		xy_out(ibrev+1) = xy_in(i+1)
	    enddo

!	    // here begins the Danielson-Lanczos section
	    integer n = ipoint
	    integer l2pt = 0
	    integer mmax = 1

	    do while (n > mmax)
		integer istep = ishft(mmax, 1)
!//	double theta = -2*M_PI/istep
!//	double complex wphase_XY = cos(theta) + sin(theta)*I
		complex wphase_XY = phasevec(l2pt)
		l2pt=l2pt+1

		complex w_XY = complex(1.0, 0.0)
		do m=1, mmax
		    do i=m, n, istep do
			complex tempXY = w_XY * xy_out(i+mmax)
			xy_out(i+mmax) = xy_out(i) - tempXY
			xy_out(i     ) = xy_out(i) + tempXY
		    enddo
		    w_XY = w_XY * wphase_XY;
		enddo
		mmax=istep
	    enddo
	  end function
	end module
