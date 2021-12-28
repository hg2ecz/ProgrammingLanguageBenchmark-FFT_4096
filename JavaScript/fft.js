"use strict"

// Internal variables
let phasevec = []

export function initialize() {
    for (let i = 0; i < 32; i++) {
        let point = 2 << i
        phasevec[i] = [Math.cos(-2 * Math.PI / point), Math.sin(-2 * Math.PI / point)]
    }
}

// Public function
export function fft(log2point, xy_in) {
    let xy_out = []

    for (let i = 0; i < (1 << log2point); i++) {
        let brev = i
        brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1)
        brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2)
        brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4)
        brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8)
        //brev = (brev >> 16) | (brev << 16)
        //brev >>= 32-log2point
        brev >>= 16 - log2point
        xy_out[brev] = [xy_in[i][0], xy_in[i][1]]
    }

    // here begins the Danielson-Lanczos section
    let n = 1 << log2point
    let l2pt = 0
    let mmax = 1

    while (n > mmax) {
        let istep = mmax << 1

        //	double theta = -2*M_PI/istep
        //	double complex wphase_XY = cos(theta) + sin(theta)*I
        let wphase_XY = phasevec[l2pt++]
        let w_XY = [1.0, 0.0]

        for (let m = 0; m < mmax; m++) {
            for (let i = m; i < n; i += istep) {
                let tempXY = [w_XY[0] * xy_out[i + mmax][0] - w_XY[1] * xy_out[i + mmax][1],
                w_XY[0] * xy_out[i + mmax][1] + w_XY[1] * xy_out[i + mmax][0]]

                xy_out[i + mmax][0] = xy_out[i][0] - tempXY[0]
                xy_out[i + mmax][1] = xy_out[i][1] - tempXY[1]

                xy_out[i][0] += tempXY[0]
                xy_out[i][1] += tempXY[1]
            }

            let w_XY_t = w_XY[0] * wphase_XY[0] - w_XY[1] * wphase_XY[1]
            w_XY[1] = w_XY[0] * wphase_XY[1] + w_XY[1] * wphase_XY[0]
            w_XY[0] = w_XY_t
        }

        mmax = istep
    }

    return xy_out
}
