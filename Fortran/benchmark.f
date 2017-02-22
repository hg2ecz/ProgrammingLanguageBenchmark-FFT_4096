	program benchmark
	use fftmod

	integer, parameter :: LOG2SIZE = 12
	integer, parameter :: FFT_REPEAT = 1000

	integer, parameter :: SIZE = lshift(1, LOG2SIZE)
	complex*16 :: xy(SIZE)
	complex*16 :: xy_out_fft(SIZE)

	real, dimension(2) :: tstart
	real, dimension(2) :: tend
	real :: startTime
	real :: endTime

	integer :: i
	do i = 1, SIZE/2
	    xy(i) = cmplx( 1.0, 0.0)
	enddo

	do i = SIZE/2+1, SIZE
	    xy(i) = cmplx(-1.0, 0.0)
	enddo
! FFT
	
	call etime(tstart, startTime)
	do i=0, FFT_REPEAT-1
	    xy_out_fft = fft(SIZE, xy)
	enddo
	call etime(tend, endTime)

	elTime_ms = (tend(1)-tstart(1))*1000.0/FFT_REPEAT
	print *, FFT_REPEAT, "piece(s) of", SIZE, "pt FFT;", elTime_ms

	do i=1, 6
	    print *, i, xy_out_fft(i)
	enddo

	end program benchmark
