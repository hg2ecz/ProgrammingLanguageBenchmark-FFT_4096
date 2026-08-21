use num_complex::Complex;
pub type Cf32 = Complex<f32>;

// From Rust 1.98
pub trait ComplexAlgebraicExt {
    fn algebraic_add(self, other: Self) -> Self;
    fn algebraic_sub(self, other: Self) -> Self;
    fn algebraic_mul(self, other: Self) -> Self;
}

impl ComplexAlgebraicExt for Complex<f32> {
    #[inline(always)]
    fn algebraic_add(self, other: Self) -> Self {
        Complex {
            re: self.re.algebraic_add(other.re),
            im: self.im.algebraic_add(other.im),
        }
    }

    #[inline(always)]
    fn algebraic_sub(self, other: Self) -> Self {
        Complex {
            re: self.re.algebraic_sub(other.re),
            im: self.im.algebraic_sub(other.im),
        }
    }

    #[inline(always)]
    fn algebraic_mul(self, other: Self) -> Self {
        Complex {
            re: self
                .re
                .algebraic_mul(other.re)
                .algebraic_sub(self.im.algebraic_mul(other.im)),
            im: self
                .im
                .algebraic_mul(other.re)
                .algebraic_add(self.re.algebraic_mul(other.im)),
        }
    }
}

impl ComplexAlgebraicExt for Complex<f64> {
    #[inline(always)]
    fn algebraic_add(self, other: Self) -> Self {
        Complex {
            re: self.re.algebraic_add(other.re),
            im: self.im.algebraic_add(other.im),
        }
    }

    #[inline(always)]
    fn algebraic_sub(self, other: Self) -> Self {
        Complex {
            re: self.re.algebraic_sub(other.re),
            im: self.im.algebraic_sub(other.im),
        }
    }

    #[inline(always)]
    fn algebraic_mul(self, other: Self) -> Self {
        Complex {
            re: self
                .re
                .algebraic_mul(other.re)
                .algebraic_sub(self.im.algebraic_mul(other.im)),
            im: self
                .im
                .algebraic_mul(other.re)
                .algebraic_add(self.re.algebraic_mul(other.im)),
        }
    }
}

pub struct Fft {
    phasevec: [Cf32; 32],
}

// Public function
impl Fft {
    pub fn new() -> Fft {
        const PHLEN: usize = 32;
        let mut fft = Fft {
            phasevec: [Cf32::new(0.0, 0.0); PHLEN],
        };
        for i in 0..PHLEN {
            let phase = -2.0 * std::f32::consts::PI / 2.0_f32.powi(i as i32 + 1);
            fft.phasevec[i] = Cf32::new(phase.cos(), phase.sin());
        }
        fft
    }

    #[inline(never)]
    pub fn fft(&self, xy_out: &mut [Cf32], xy_in: &[Cf32]) {
        let log2point = xy_in.len().ilog2();
        // if we use these assert_eq checks, the compiler can produce a faster code
        assert_eq!(xy_out.len(), xy_in.len());
        assert_eq!(xy_in.len(), 1 << log2point);

        for (i, &xy_act) in xy_in.iter().enumerate() {
            xy_out[i.reverse_bits() >> (usize::count_zeros(0) - log2point)] = xy_act;
        }

        // here begins the Danielson-Lanczos section;
        for l2pt in 0..log2point {
            let wphase_xy = self.phasevec[l2pt as usize];
            let mmax = 1 << l2pt;
            let mut w_xy = Cf32::new(1.0, 0.0);
            for m in 0..mmax {
                for i in (m..xy_out.len()).step_by(mmax << 1) {
                    let temp = w_xy.algebraic_mul(xy_out[i + mmax]);
                    xy_out[i + mmax] = xy_out[i].algebraic_sub(temp);
                    xy_out[i] = xy_out[i].algebraic_add(temp);
                }
                w_xy = w_xy.algebraic_mul(wphase_xy); // rotate
            }
        }
    }
}
