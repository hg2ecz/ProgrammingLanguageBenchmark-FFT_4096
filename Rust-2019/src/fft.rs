use num_complex::Complex;

pub struct Fft {
    phasevec: Vec<Complex<f64>>,
}

// Public function
impl Fft {
    pub fn new() -> Fft {
	let mut fft = Fft { phasevec: Vec::new() };
	for i in 0..32 {
	    let point: i32 = 2<<i;
	    fft.phasevec.push( Complex::new(
		(-2.0*std::f64::consts::PI/(point as f64)).cos(),
		(-2.0*std::f64::consts::PI/(point as f64)).sin()
	    ) );
	}
	fft
    }

    pub fn fft(&self, log2point: u32, xy_out: &mut [Complex<f64>], xy_in: &[Complex<f64>]) {
	for i in 0 .. 1_usize << log2point {
	    let brev = i.reverse_bits() >> (usize::count_zeros(0)-log2point);
	    xy_out[brev] = xy_in[i];
	}

	// here begins the Danielson-Lanczos section;
	for l2pt in 0..log2point {
	    let wphase_xy = self.phasevec[l2pt as usize];
	    let mmax = 1<<l2pt;
	    let mut w_xy = Complex::new(1.0, 0.0);
	    for m in 0..mmax {
		for i in (m..1<<log2point).step_by(mmax<<1) {
		    let temp = w_xy * xy_out[i+mmax];
		    xy_out[i+mmax] = xy_out[i] - temp;
		    xy_out[i     ] += temp;
		}
		w_xy *= wphase_xy; // rotate
	    }
	}
    }
}
