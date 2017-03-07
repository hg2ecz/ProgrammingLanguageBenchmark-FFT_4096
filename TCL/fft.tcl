# Internal variables
set phasevec_exist 0
array set phasevec {}

# Public function
proc fft {log2point xy_out_ref xy_in_ref} {
    global phasevec_exist
    global phasevec
    upvar $xy_out_ref xy_out
    upvar $xy_in_ref xy_in
#    parray {xy_in}

    if { $phasevec_exist == 0 } {
	set pi [expr atan(1)*4]
	for {set i 0} {$i < 32} {incr i} {
	    set phasevec($i-0) [expr cos(-2*$pi/(2<<$i))]
	    set phasevec($i-1) [expr sin(-2*$pi/(2<<$i))]
	}
	set $phasevec_exist 1
    }

    set point [expr 1<<$log2point]

    for {set i 0} {$i < $point} {incr i} {
	set brev $i
	set brev [expr (($brev & 0xaaaaaaaa) >> 1) | (($brev & 0x55555555) << 1)]
	set brev [expr (($brev & 0xcccccccc) >> 2) | (($brev & 0x33333333) << 2)]
	set brev [expr (($brev & 0xf0f0f0f0) >> 4) | (($brev & 0x0f0f0f0f) << 4)]
	set brev [expr (($brev & 0xff00ff00) >> 8) | (($brev & 0x00ff00ff) << 8)]
	set brev [expr ($brev >> 16) | ($brev << 16)]

	set brev [expr $brev >> (32-$log2point)]
	set xy_out($brev-0) $xy_in($i-0);
	set xy_out($brev-1) $xy_in($i-1);
    }

    # here begins the Danielson-Lanczos section
    set n [expr 1<<$log2point]
    set l2pt 0
    set mmax 1
    while {$n > $mmax} {
	set istep [expr $mmax<<1]

#	double theta = -2*M_PI/istep
#	double complex wphase_XY = cos(theta) + sin(theta)*I

#	parray {phasevec}
	set wphaseX $phasevec($l2pt-0)
	set wphaseY $phasevec($l2pt-1)
	incr l2pt

	set wX 1.0
	set wY 0.0
	#puts [lindex $w_XY 0]
	for {set m 0} {$m < $mmax} {incr m} {
	    for {set i $m} {$i < $n} {set i [expr $i+$istep]} {
		set tempX [expr $wX * $xy_out([expr $i+$mmax]-0) - $wY * $xy_out([expr $i+$mmax]-1) ]
		set tempY [expr $wX * $xy_out([expr $i+$mmax]-1) + $wY * $xy_out([expr $i+$mmax]-0) ]

		set xy_out([expr $i+$mmax]-0)  [expr $xy_out($i-0) - $tempX]
		set xy_out([expr $i+$mmax]-1)  [expr $xy_out($i-1) - $tempY]

		set xy_out($i-0)               [expr $xy_out($i-0) + $tempX]
		set xy_out($i-1)               [expr $xy_out($i-1) + $tempY]
	    }
	    set wX_t  [expr $wX * $wphaseX - $wY * $wphaseY]
	    set wY    [expr $wX * $wphaseY + $wY * $wphaseX]
	    set wX $wX_t
	}
	set mmax $istep
    }
}
