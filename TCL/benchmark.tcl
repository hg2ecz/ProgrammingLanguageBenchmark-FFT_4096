#!/usr/bin/tclsh

source "fft.tcl"

set LOG2FFTSIZE 12
set FFT_REPEAT  2

set SIZE [expr 1<<$LOG2FFTSIZE]
array set xy {}

proc main {LOG2FFTSIZE &xy FFT_REPEAT} {
    set SIZE [expr 1<<$LOG2FFTSIZE]
    array set fft_out {}

    for {set i 0} {$i < $SIZE/2} {incr i} {
	set xy($i-0)      1.0
	set xy($i-1)      0.0
	set fft_out($i-0) 0.0
	set fft_out($i-1) 0.0
    }

    for {set i [expr $SIZE/2]} {$i < $SIZE} {incr i} {
	set xy($i-0)     -1.0
	set xy($i-1)      0.0
	set fft_out($i-0) 0.0
	set fft_out($i-1) 0.0
    }

#    parray {xy}
#    parray {fft_out}

    set timestart [clock clicks -millisec]
    for {set i 0} {$i < $FFT_REPEAT} {incr i} {
	fft $LOG2FFTSIZE fft_out xy
    }

    set eltime [expr { [clock clicks -millisec] - $timestart }]
    puts "$FFT_REPEAT piece of $SIZE pt FFT; [expr {$eltime/double($FFT_REPEAT)}] ms/piece"

    for {set i 1} {$i < 7} {incr i} {
	puts "$i $fft_out($i-0) $fft_out($i-1)"
    }
}

main $LOG2FFTSIZE xy $FFT_REPEAT