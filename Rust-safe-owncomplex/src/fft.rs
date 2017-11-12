use std;

pub struct Cplx {
    pub re: f64,
    pub im: f64,
}

impl Copy for Cplx { }
impl Clone for Cplx {
    fn clone(&self) -> Cplx {
        *self
    }
}

pub fn cadd(a: Cplx, b: Cplx) -> Cplx {
    let re = a.re + b.re;
    let im = a.im + b.im;
    Cplx {re, im}
}

pub fn csub(a: Cplx, b: Cplx) -> Cplx {
    let re = a.re - b.re;
    let im = a.im - b.im;
    Cplx {re, im}
}

pub fn cmul(a: Cplx, b: Cplx) -> Cplx {
    let re = a.re * b.re - a.im * b.im;
    let im = a.re * b.im + a.im * b.re;
    Cplx {re, im}
}

pub struct Fft {
    phasevec: [Cplx; 32]   // = [Cplx{re: 0.0, im: 0.0}; 32],
}

impl Fft {
  pub fn new() -> Fft {
    let mut fft: Fft = Fft {phasevec: [Cplx{re:0.0, im: 0.0}; 32]};
    for i in 0..32 {
	let point: i32 = 2<<i;
	fft.phasevec[i].re = (-2.0*std::f64::consts::PI/(point as f64)).cos();
	fft.phasevec[i].im = (-2.0*std::f64::consts::PI/(point as f64)).sin();
    }
    fft
  }

  // Public function
  pub fn fft(&self, log2point: u32, xy_out: &mut[Cplx], xy_in: &[Cplx]) {
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
	let wphase_xy: Cplx;
	wphase_xy = self.phasevec[l2pt];
	l2pt+=1;
	let mut w_xy: Cplx = Cplx {re: 1.0, im: 0.0};
	for m in 0..mmax {;
	    let mut i = m;
	    while i < n {
		let temp: Cplx = cmul(w_xy, xy_out[i+mmax]);
		xy_out[i+mmax] = csub(xy_out[i], temp);
		xy_out[i     ] = cadd(xy_out[i], temp);
	        i += istep;
	    }
	    w_xy = cmul(w_xy, wphase_xy); // rotate
	}
	mmax=istep;
    }
  }
} // impl
