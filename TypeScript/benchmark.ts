#!/usr/local/bin/tsc

import Fft from './fft';

var LOG2FFTSIZE: number = 12;
var FFT_REPEAT: number = 1000;

var xy: number[][] = [];
var xy_out: number[][] = [];

function main() {
    var i: number;
    var eltime: number;
    var SIZE: number = 1 << LOG2FFTSIZE;

    for (i = 0; i < SIZE / 2; i++) xy[i] = [1., 0];
    for (; i < SIZE; i++) xy[i] = [-1., 0];

    // FFT
    var starttime: Date = new Date();
    for (i = 0; i < FFT_REPEAT; i++) xy_out = Fft.Fft(LOG2FFTSIZE, xy);
    var endtime: Date = new Date();
    eltime = endtime.getTime() - starttime.getTime(); // msec
    console.log("\n--> " + FFT_REPEAT + " piece of " + (1 << LOG2FFTSIZE) + " pt FFT;  " + (eltime / FFT_REPEAT) + " ms/piece");

    for (i = 0; i < 6; i++) {
        console.log(i + "\t" + xy_out[i][0] + "\t" + xy_out[i][1]);
    }
    return 0;
}

main();
