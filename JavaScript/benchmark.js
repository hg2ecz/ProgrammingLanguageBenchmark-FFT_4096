#!/usr/bin/nodejs

"use strict";

var fft = require('./fft');


var LOG2FFTSIZE=12
var FFT_REPEAT=1000

var xy = []
var xy_out = []

function main() {
    var i
    var eltime
    var SIZE=1<<LOG2FFTSIZE

    for(i=0; i<SIZE/2; i++) xy[i]= [1., 0]
    for(   ; i<SIZE  ; i++) xy[i]=[-1., 0]

// FFT
    var starttime = new Date()
    for (i=0; i<FFT_REPEAT; i++) xy_out = fft.fft(LOG2FFTSIZE, xy)
    var endtime = new Date()
    eltime = endtime - starttime; // msec
    console.log("\n--> " + FFT_REPEAT + " piece of "+ (1<<LOG2FFTSIZE)+" pt FFT;  "+ (eltime/FFT_REPEAT)+" ms/piece")

    for(i=0; i<6; i++) {
        console.log(i + "\t"+xy_out[i][0]+"\t"+xy_out[i][1])
    }
    return 0
}

main()
