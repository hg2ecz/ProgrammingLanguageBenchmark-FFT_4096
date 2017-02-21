	program benchmark
	use fftmod

	integer, parameter :: LOG2SIZE = 12
	integer, parameter :: FFT_REPEAT = 1000

	integer, parameter :: SIZE = lshift(1, LOG2SIZE)
	complex :: xy(SIZE)
	complex :: xy_out_fft(SIZE)

	real, dimension(2) :: tstart
	real, dimension(2) :: tend
	real :: startTime
	real :: endTime

	integer :: i = 0
	do i = 1, SIZE/2-1
	    xy(i) = cmplx( 1.0, 0.0)
	enddo

	do i = SIZE/2, SIZE-1
	    xy(i) = cmplx(-1.0, 0.0)
	enddo
! FFT
	
	call etime(tstart, startTime)
	do i=0, FFT_REPEAT
	    xy_out_fft = fft(SIZE, xy)
	enddo
	call etime(tend, endTime)
	real elTime = endTime-startTime

	print *, FFT_REPEAT, "piece(s) of", SIZE, "pt FFT;    ", elTime/FFT_REPEAT, "ms/piece\n"

	do i=0, 6-1
	    print *, i, xy_out_fft(i)
	enddo

	end program benchmark
