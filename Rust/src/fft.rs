use num_complex::Complex;

pub struct Fft {
    phasevec: [Complex<f32>; 32],
}

// Public function
impl Fft {
    pub fn new() -> Fft {
        const PHLEN: usize = 32;
        let mut fft = Fft {
            phasevec: [Complex::new(0.0, 0.0); PHLEN],
        };
        for i in 0..PHLEN {
            let phase = -2.0 * std::f32::consts::PI / 2.0_f32.powi(i as i32 + 1);
            fft.phasevec[i] = Complex::new(phase.cos(), phase.sin());
        }
        fft
    }

    pub fn fft(&self, xy_out: &mut [Complex<f32>], xy_in: &[Complex<f32>]) {
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
            let mut w_xy = Complex::new(1.0, 0.0);
            for m in 0..mmax {
                for i in (m..xy_out.len()).step_by(mmax << 1) {
                    let temp = w_xy * xy_out[i + mmax];
                    xy_out[i + mmax] = xy_out[i] - temp;
                    xy_out[i] += temp;
                }
                w_xy *= wphase_xy; // rotate
            }
        }
    }
}
