#!/usr/bin/php

<?php
include "fft.php";

$LOG2FFTSIZE = 16;
$FFT_REPEAT = 10;

$SIZE = 1<<$LOG2FFTSIZE;
$xy = new SplFixedArray($SIZE);

function main($LOG2FFTSIZE, $xy, $FFT_REPEAT) {
    $SIZE = 1<<$LOG2FFTSIZE;

    for ($i=0; $i<$SIZE/2; $i++) $xy[$i]= array(1., 0);
    for (    ; $i<$SIZE  ; $i++) $xy[$i]= array(-1., 0);

    $timestart = microtime(True);
    for ($i=0; $i<$FFT_REPEAT; $i++) $fft_out = fft($LOG2FFTSIZE, $xy);
    $eltime = 1000.*(time(True) - $timestart);
    printf("%6d piece of %d pt FFT;  %9.5f ms/piece\n", $FFT_REPEAT, 1<<$LOG2FFTSIZE, $eltime/$FFT_REPEAT);

    for ($i=0; $i<6; $i++) printf("%3d %16.9f %16.9f\n", $i, $fft_out[$i][0], $fft_out[$i][1]);

}

main($LOG2FFTSIZE, $xy, $FFT_REPEAT);
?>
