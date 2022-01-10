#!/usr/bin/nodejs

"use strict";

import { fft, initialize } from './fft.js';

const LOG2FFTSIZE = 12
const FFT_REPEAT = 1000

function main() {
    const SIZE = 1 << LOG2FFTSIZE
    const xy = []
    let xy_out = []    
    
    for (var i = 0; i < SIZE / 2; i++) xy[i] = [1., 0]
    for (; i < SIZE; i++) xy[i] = [-1., 0]
    initialize()

    // FFT
    const starttime = new Date()
    for (i = 0; i < FFT_REPEAT; i++) xy_out = fft(LOG2FFTSIZE, xy)

    const endtime = new Date()
    const eltime = endtime - starttime; // msec
    console.log("\n--> " + FFT_REPEAT + " piece of " + (1 << LOG2FFTSIZE) + " pt FFT;  " + (eltime / FFT_REPEAT) + " ms/piece")

    for (i = 0; i < 6; i++) {
        console.log(i + "\t" + xy_out[i][0] + "\t" + xy_out[i][1])
    }

    return 0
}

main()
