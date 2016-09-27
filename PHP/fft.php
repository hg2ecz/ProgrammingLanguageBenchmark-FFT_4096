<?php

// Internal variables
$phasevec_exist = 0;
$phasevec = new SplFixedArray(30);

// Public function
function fft($log2point, $xy_in) {
    global $phasevec, $phasevec_exist;
    if (!$phasevec_exist) {
	for ($i=0; $i<30; $i++) { // 2**32 --> negative !!! 32 bit signed
	    $point = 2<<$i;
	    $phasevec[$i] = array(cos(-2*M_PI/$point), sin(-2*M_PI/$point)); // complex.h ??
	}
	$phasevec_exist = 1;
    }

    //$xy_out = new SplFixedArray(1<<$log2point);
    $xy_out = array();
    for ($i=0; $i < (1<<$log2point); $i++) {
	$brev = $i;
	$brev = (($brev & 0xaaaaaaaa) >> 1) | (($brev & 0x55555555) << 1);
	$brev = (($brev & 0xcccccccc) >> 2) | (($brev & 0x33333333) << 2);
	$brev = (($brev & 0xf0f0f0f0) >> 4) | (($brev & 0x0f0f0f0f) << 4);
	$brev = (($brev & 0xff00ff00) >> 8) | (($brev & 0x00ff00ff) << 8);
	// $brev = ($brev >> 16) | ($brev << 16); -- PHP: max 30 bit (signed int)

	//$brev >>= 32-$log2point;
	$brev >>= 16-$log2point;   // no >>16 <<16
	$xy_out[$brev] = $xy_in[$i];
    }

    // here begins the Danielson-Lanczos section
    $n = 1<<$log2point;
    $l2pt=0;
    $mmax=1;
    while ($n > $mmax) {
	$istep = $mmax<<1;

	$wphase_XY = $phasevec[$l2pt++];

	$w_XY = array(1.0, 0.0);
	for ($m=0; $m < $mmax; $m++) {
	    for ($i=$m; $i < $n; $i += $istep) {
		$tempXY[0] = $w_XY[0] * $xy_out[$i+$mmax][0] - $w_XY[1] * $xy_out[$i+$mmax][1]; // complex.h ??
		$tempXY[1] = $w_XY[0] * $xy_out[$i+$mmax][1] + $w_XY[1] * $xy_out[$i+$mmax][0]; // complex.h ??

		$xy_out[$i+$mmax][0]  = $xy_out[$i][0] - $tempXY[0];
		$xy_out[$i+$mmax][1]  = $xy_out[$i][1] - $tempXY[1];

		$xy_out[$i     ][0] += $tempXY[0];
		$xy_out[$i     ][1] += $tempXY[1];
	    }
	    $w_XY_t  = $w_XY[0] * $wphase_XY[0] - $w_XY[1] * $wphase_XY[1]; // rotate
	    $w_XY[1] = $w_XY[0] * $wphase_XY[1] + $w_XY[1] * $wphase_XY[0]; // rotate
	    $w_XY[0] = $w_XY_t;
	}
	$mmax=$istep;
    }
    return $xy_out;
}
?>
