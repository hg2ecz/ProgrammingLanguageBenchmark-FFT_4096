use std;

// Internal variables
static mut PHASEVEC_EXIST : bool = false;
static mut PHASEVEC: [[f64; 2]; 32] = [[0.0; 2]; 32];

// Public function
pub unsafe fn fft(log2point: u32, xy_in: [[f64; 2]; 4096]) -> [[f64; 2]; 4096] {
    let mut xy_out: [[f64; 2]; 4096] = [[0.0; 2]; 4096];

    if !PHASEVEC_EXIST {;
	for i in 0..32 {
	    let point: i32 = 2<<i;
	    PHASEVEC[i][0] = (-2.0*std::f64::consts::PI/(point as f64)).cos();
	    PHASEVEC[i][1] = (-2.0*std::f64::consts::PI/(point as f64)).sin();
	}
	PHASEVEC_EXIST = true;
    };
    for i in 0 .. 1<<log2point {
	let mut brev: usize = i;
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
    let mut l2pt: usize = 0;
    let mut mmax: usize = 1;

    while n > mmax {;
	let istep: usize = mmax<<1;
//	double theta = -2*M_PI/istep;
//	double complex wphase_xy = cos(theta) + sin(theta)*I;
	let wphase_xy: [f64; 2] = PHASEVEC[l2pt];
	l2pt+=1;
	let mut w_xy: [f64; 2] = [1.0, 0.0];
	for m in 0..mmax {;
//	    for i in (m .. n).step_by(istep) { -- now unstable
	    let mut i = m;
	    while i < n {
//		let temp_xY: [f64; 2] = w_xy * xy_out[i+mmax]; -- missing complex arithmetic
//		xy_out[i+mmax]  = xy_out[i] - temp_xY;
//		xy_out[i     ] += temp_xY;
		let temp_x: f64 = w_xy[0] * xy_out[i+mmax][0] - w_xy[1] * xy_out[i+mmax][1];
		let temp_y: f64 = w_xy[0] * xy_out[i+mmax][1] + w_xy[1] * xy_out[i+mmax][0];
		xy_out[i+mmax][0]  = xy_out[i][0] - temp_x;
		xy_out[i+mmax][1]  = xy_out[i][1] - temp_y;
		xy_out[i     ][0] += temp_x;
		xy_out[i     ][1] += temp_y;
	        i += istep;
	    }
//	    w_xy *= wphase_xy; // rotate -- missing complex arithmetic
	    let w_tmp = w_xy[0] * wphase_xy[0] - w_xy[1] * wphase_xy[1];
	    w_xy[1] = w_xy[0] * wphase_xy[1] + w_xy[1] * wphase_xy[0];
	    w_xy[0] = w_tmp;
	}
	mmax=istep;
    }
    return xy_out;
}
