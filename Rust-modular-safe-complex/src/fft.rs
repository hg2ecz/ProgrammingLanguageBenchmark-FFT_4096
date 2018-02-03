extern crate num_complex;
use num_complex::Complex;

pub struct Fft {
    phasevec: [Complex<f64>; 32]
}

// Public function
impl Fft {
    pub fn new() -> Fft {
	let mut pvec: [Complex<f64>; 32] = [Complex::new(0.0, 0.0); 32];
	for i in 0..32 {
	    let point: i32 = 2<<i;
	    pvec[i] = Complex::new(
		(-2.0*std::f64::consts::PI/(point as f64)).cos(),
		(-2.0*std::f64::consts::PI/(point as f64)).sin()
	    );
	}
	Fft { phasevec: pvec}
    }

    pub fn fft(&self, log2point: u32, xy_out: &mut [Complex<f64>], xy_in: &[Complex<f64>]) {
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

	while n > mmax {
	    let istep: usize = mmax<<1;
	    let wphase_xy: Complex<f64>;
	    wphase_xy = self.phasevec[l2pt];
	    l2pt+=1;
	    let mut w_xy: Complex<f64> = Complex::new(1.0, 0.0);

	    for m in 0..mmax {;
		let mut i = m;
		while i < n {
		    let temp: Complex<f64> = w_xy * xy_out[i+mmax];
		    xy_out[i+mmax] = xy_out[i] - temp;
		    xy_out[i     ] += temp;
		    i += istep;
		}
		w_xy *= wphase_xy; // rotate
	    }
	    mmax=istep;
	}
    }
}
