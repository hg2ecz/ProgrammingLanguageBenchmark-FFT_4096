#!/usr/bin/nodejs

"use strict";

import { fft, initialize } from './fft.js';

let LOG2FFTSIZE = 12
let FFT_REPEAT = 1000

let xy = []
let xy_out = []

function main() {
    let SIZE = 1 << LOG2FFTSIZE

    for (var i = 0; i < SIZE / 2; i++) xy[i] = [1., 0]
    for (; i < SIZE; i++) xy[i] = [-1., 0]
    initialize()

    // FFT
    let starttime = new Date()
    for (i = 0; i < FFT_REPEAT; i++) xy_out = fft(LOG2FFTSIZE, xy)

    let endtime = new Date()
    let eltime = endtime - starttime; // msec
    console.log("\n--> " + FFT_REPEAT + " piece of " + (1 << LOG2FFTSIZE) + " pt FFT;  " + (eltime / FFT_REPEAT) + " ms/piece")

    for (i = 0; i < 6; i++) {
        console.log(i + "\t" + xy_out[i][0] + "\t" + xy_out[i][1])
    }

    return 0
}

main()
