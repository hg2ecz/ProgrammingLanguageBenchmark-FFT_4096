use std::ops;

#[derive(Copy, Clone)]
pub struct Cplx<T> {
    pub re: T,
    pub im: T,
}

impl<T> ops::Add<Cplx<T>> for Cplx<T>
where
    T: ops::Add<Output = T>,
{
    type Output = Cplx<T>;
    fn add(self, b: Cplx<T>) -> Cplx<T> {
        let re = self.re + b.re;
        let im = self.im + b.im;
        Cplx { re, im }
    }
}

impl<T> ops::Sub<Cplx<T>> for Cplx<T>
where
    T: ops::Sub<Output = T>,
{
    type Output = Cplx<T>;
    fn sub(self, b: Cplx<T>) -> Cplx<T> {
        let re = self.re - b.re;
        let im = self.im - b.im;
        Cplx { re, im }
    }
}

impl<T> ops::Mul<Cplx<T>> for Cplx<T>
where
    T: ops::Mul<Output = T> + ops::Add<Output = T> + ops::Sub<Output = T> + Copy,
{
    type Output = Cplx<T>;
    fn mul(self, b: Cplx<T>) -> Cplx<T> {
        let re = self.re * b.re - self.im * b.im;
        let im = self.re * b.im + self.im * b.re;
        Cplx { re, im }
    }
}

pub struct Fft {
    phasevec: [Cplx<f64>; 32],
}

impl Fft {
    pub fn new() -> Fft {
        let mut fft: Fft = Fft {
            phasevec: [Cplx { re: 0.0, im: 0.0 }; 32],
        };
        for i in 0..fft.phasevec.len() {
            let point: i32 = 2 << i;
            fft.phasevec[i].re = (-2.0 * std::f64::consts::PI / (point as f64)).cos();
            fft.phasevec[i].im = (-2.0 * std::f64::consts::PI / (point as f64)).sin();
        }
        fft
    }

    // Public function
    pub fn fft(&self, log2point: u32, xy_out: &mut [Cplx<f64>], xy_in: &[Cplx<f64>]) {
        for (i, &item) in xy_in.iter().enumerate().take(1 << log2point) {
            //let mut brev: usize = i;
            //brev = ((brev & 0xaaaa_aaaa) >> 1) | ((brev & 0x5555_5555) << 1);
            //brev = ((brev & 0xcccc_cccc) >> 2) | ((brev & 0x3333_3333) << 2);
            //brev = ((brev & 0xf0f0_f0f0) >> 4) | ((brev & 0x0f0f_0f0f) << 4);
            //brev = ((brev & 0xff00_ff00) >> 8) | ((brev & 0x00ff_00ff) << 8);
            //brev = (brev >> 16) | (brev << 16);
            //brev >>= 32 - log2point;
            let brev = i.reverse_bits() >> (usize::count_zeros(0) - log2point);
            xy_out[brev] = item; // xy_in[i]
        }

        // here begins the Danielson-Lanczos section;
        let n: usize = 1 << log2point;
        let mut l2pt: usize = 0;
        let mut mmax: usize = 1;

        while n > mmax {
            let istep: usize = mmax << 1;
            let wphase_xy = self.phasevec[l2pt];
            l2pt += 1;
            let mut w_xy = Cplx { re: 1.0, im: 0.0 };
            for m in 0..mmax {
                let mut i = m;
                while i < n {
                    let temp = w_xy * xy_out[i + mmax];
                    xy_out[i + mmax] = xy_out[i] - temp;
                    xy_out[i] = xy_out[i] + temp;
                    i += istep;
                }
                w_xy = w_xy * wphase_xy; // rotate
            }
            mmax = istep;
        }
    }
} // impl
