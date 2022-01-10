#!/usr/local/bin/tsc

import Fft from './fft';

const LOG2FFTSIZE: number = 12;
const FFT_REPEAT: number = 1000;

function main() {
    const SIZE: number = 1 << LOG2FFTSIZE;
    const xy: number[][] = [];
    let xy_out: number[][] = [];    
    
    for (var i = 0; i < SIZE / 2; i++) xy[i] = [1., 0];
    for (; i < SIZE; i++) xy[i] = [-1., 0];
    const fft = new Fft();

    // FFT
    const starttime: Date = new Date();

    for (let i = 0; i < FFT_REPEAT; i++) xy_out = fft.calculate(LOG2FFTSIZE, xy);

    const endtime: Date = new Date();
    const eltime = endtime.getTime() - starttime.getTime(); // msec
    console.log("\n--> " + FFT_REPEAT + " piece of " + (1 << LOG2FFTSIZE) + " pt FFT;  " + (eltime / FFT_REPEAT) + " ms/piece");

    for (let i = 0; i < 6; i++) {
        console.log(i + "\t" + xy_out[i][0] + "\t" + xy_out[i][1]);
    }

    return 0;
}

main();
