use std;
use std::mem;

// Internal variables
static phasevec_exist : bool = false;
static phasevec: [[f64; 2]; 32] = std::mem::uninitialized();

// Public function
pub fn fft(log2point: u32, xy_out: [[f64; 2]; 4096], xy_in: [[f64; 2]; 4096]) {
    let i: u32;
    if !phasevec_exist {;
	for i in 0..32 {
	    let point: i32 = 2<<i;
	    phasevec[i][0] = (-2.0*std::f64::consts::PI/(point as f64)).cos();
	    phasevec[i][1] = (-2.0*std::f64::consts::PI/(point as f64)).sin();
	}
	phasevec_exist = true;
    };
    for i in 0 .. 1<<log2point {
	let brev: usize = i;
	brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1);
	brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2);
	brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4);
	brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8);
	brev = (brev >> 16) | (brev << 16);

	brev >>= 32-log2point;
	xy_out[brev] = xy_in[i];
    }

    // here begins the Danielson-Lanczos section;
    let n: usize = 1<<log2point;
    let l2pt: usize = 0;
    let mmax: usize = 1;

    while n > mmax {;
	let istep: usize = mmax<<1;
//	double theta = -2*M_PI/istep;
//	double complex wphase_XY = cos(theta) + sin(theta)*I;
	let wphase_XY: [f64; 2] = phasevec[l2pt];
	l2pt+=1;
	let w_XY: [f64; 2] = [1.0, 0.0];
	for m in 0..mmax {;
//	    for i in (m .. n).step_by(istep) { -- now unstable
	    let i = m;
	    while i < n {
//		let tempXY: [f64; 2] = w_XY * xy_out[i+mmax]; -- missing complex arithmetic
//		xy_out[i+mmax]  = xy_out[i] - tempXY;
//		xy_out[i     ] += tempXY;
		let tempX: f64 = w_XY[0] * xy_out[i+mmax][0] - w_XY[1] * xy_out[i+mmax][1];
		let tempY: f64 = w_XY[0] * xy_out[i+mmax][1] + w_XY[1] * xy_out[i+mmax][0];
		xy_out[i+mmax][0]  = xy_out[i][0] - tempX;
		xy_out[i+mmax][1]  = xy_out[i][1] - tempY;
		xy_out[i     ][0] += tempX;
		xy_out[i     ][1] += tempY;
	        i += istep;
	    }
//	    w_XY *= wphase_XY; // rotate -- missing complex arithmetic
	    let w_tmp = w_XY[0] * wphase_XY[0] - w_XY[1] * wphase_XY[1];
	    w_XY[1] = w_XY[0] * wphase_XY[1] + w_XY[1] * wphase_XY[0];
	    w_XY[0] = w_tmp;
	}
	mmax=istep;
    }
}
