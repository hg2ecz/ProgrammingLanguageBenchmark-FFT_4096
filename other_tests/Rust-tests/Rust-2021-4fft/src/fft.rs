use num_complex::Complex;
use std::ops;

#[derive(Copy, Clone)]
pub struct Complex4 {
    re: [f32; 4],
    im: [f32; 4],
}

impl ops::Add<Complex4> for Complex4 {
    type Output = Complex4;
    fn add(self, rhs: Complex4) -> Complex4 {
        Complex4 {
            re: [
                self.re[0] + rhs.re[0],
                self.re[1] + rhs.re[1],
                self.re[2] + rhs.re[2],
                self.re[3] + rhs.re[3],
            ],
            im: [
                self.im[0] + rhs.im[0],
                self.im[1] + rhs.im[1],
                self.im[2] + rhs.im[2],
                self.im[3] + rhs.im[3],
            ],
        }
    }
}

impl ops::AddAssign<Complex4> for Complex4 {
    fn add_assign(&mut self, rhs: Complex4) {
        self.re[0] += rhs.re[0];
        self.re[1] += rhs.re[1];
        self.re[2] += rhs.re[2];
        self.re[3] += rhs.re[3];
        self.im[0] += rhs.im[0];
        self.im[1] += rhs.im[1];
        self.im[2] += rhs.im[2];
        self.im[3] += rhs.im[3];
    }
}

impl ops::Sub<Complex4> for Complex4 {
    type Output = Complex4;
    fn sub(self, rhs: Complex4) -> Complex4 {
        Complex4 {
            re: [
                self.re[0] - rhs.re[0],
                self.re[1] - rhs.re[1],
                self.re[2] - rhs.re[2],
                self.re[3] - rhs.re[3],
            ],
            im: [
                self.im[0] - rhs.im[0],
                self.im[1] - rhs.im[1],
                self.im[2] - rhs.im[2],
                self.im[3] - rhs.im[3],
            ],
        }
    }
}

impl ops::Mul<Complex4> for Complex4 {
    type Output = Complex4;
    fn mul(self, rhs: Complex4) -> Complex4 {
        Complex4 {
            re: [
                self.re[0] * rhs.re[0] - self.im[0] * rhs.im[0],
                self.re[1] * rhs.re[1] - self.im[1] * rhs.im[1],
                self.re[2] * rhs.re[2] - self.im[2] * rhs.im[2],
                self.re[3] * rhs.re[3] - self.im[3] * rhs.im[3],
            ],
            im: [
                self.im[0] * rhs.re[0] + self.re[0] * rhs.im[0],
                self.im[1] * rhs.re[1] + self.re[1] * rhs.im[1],
                self.im[2] * rhs.re[2] + self.re[2] * rhs.im[2],
                self.im[3] * rhs.re[3] + self.re[3] * rhs.im[3],
            ],
        }
    }
}

impl ops::MulAssign<Complex4> for Complex4 {
    fn mul_assign(&mut self, rhs: Complex4) {
        let tmp_re = self.re;
        self.re[0] = tmp_re[0] * rhs.re[0] - self.im[0] * rhs.im[0];
        self.re[1] = tmp_re[1] * rhs.re[1] - self.im[1] * rhs.im[1];
        self.re[2] = tmp_re[2] * rhs.re[2] - self.im[2] * rhs.im[2];
        self.re[3] = tmp_re[3] * rhs.re[3] - self.im[3] * rhs.im[3];
        self.im[0] = self.im[0] * rhs.re[0] + tmp_re[0] * rhs.im[0];
        self.im[1] = self.im[1] * rhs.re[1] + tmp_re[1] * rhs.im[1];
        self.im[2] = self.im[2] * rhs.re[2] + tmp_re[2] * rhs.im[2];
        self.im[3] = self.im[3] * rhs.re[3] + tmp_re[3] * rhs.im[3];
    }
}

pub struct Fft {
    phasevec: Vec<Complex4>,
}

// Public function
impl Fft {
    pub fn new() -> Fft {
        let mut fft = Fft { phasevec: Vec::new() };
        for i in 0..32 {
            let point: i32 = 2 << i;
            let ph_re = (-2.0 * std::f32::consts::PI / (point as f32)).cos();
            let ph_im = (-2.0 * std::f32::consts::PI / (point as f32)).sin();
            fft.phasevec.push(Complex4 {
                re: [ph_re, ph_re, ph_re, ph_re],
                im: [ph_im, ph_im, ph_im, ph_im],
            });
        }
        fft
    }

    // do not optimize with the caller func
    #[inline(never)]
    pub fn fft(&self, log2point: u32, xy_out4: &mut [[Complex<f32>; 4096]; 4], xy_in: &[[Complex<f32>; 4096]; 4]) {
        // complex to simd
        let mut xy_out: Vec<Complex4> = vec![
            Complex4 {
                re: [0., 0., 0., 0.],
                im: [0., 0., 0., 0.]
            };
            1 << log2point
        ];

        // fft rearrange
        for i in 0..1_usize << log2point {
            let brev = i.reverse_bits() >> (usize::count_zeros(0) - log2point);
            xy_out[brev] = Complex4 {
                re: [xy_in[0][i].re, xy_in[1][i].re, xy_in[2][i].re, xy_in[3][i].re],
                im: [xy_in[0][i].im, xy_in[1][i].im, xy_in[2][i].im, xy_in[3][i].im],
            };
        }

        // here begins the Danielson-Lanczos section;
        for l2pt in 0..log2point {
            let wphase_xy = self.phasevec[l2pt as usize];
            let mmax = 1 << l2pt;
            let mut w_xy = Complex4 {
                re: [1.0, 1.0, 1.0, 1.0],
                im: [0.0, 0.0, 0.0, 0.0],
            };
            for m in 0..mmax {
                for i in (m..1 << log2point).step_by(mmax << 1) {
                    let temp = w_xy * xy_out[i + mmax];
                    xy_out[i + mmax] = xy_out[i] - temp;
                    xy_out[i] += temp;
                }
                w_xy *= wphase_xy; // rotate
            }
        }

        // simd to complex
        for (i, xyo) in xy_out.iter().enumerate().take(1 << log2point) {
            xy_out4[0][i] = Complex {
                re: xyo.re[0],
                im: xyo.im[0],
            };
            xy_out4[1][i] = Complex {
                re: xyo.re[1],
                im: xyo.im[1],
            };
            xy_out4[2][i] = Complex {
                re: xyo.re[2],
                im: xyo.im[2],
            };
            xy_out4[3][i] = Complex {
                re: xyo.re[3],
                im: xyo.im[3],
            };
        }
    }
}
