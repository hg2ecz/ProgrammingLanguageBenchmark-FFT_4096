//mod fft;        // -- if we used as module
extern crate fft; // -- if we used as lib 
extern crate num_complex;

use std::time::Instant;
use num_complex::Complex;

const LOG2FFTSIZE: u32 = 12;
const FFT_REPEAT: u32 = 1000;
const SIZE: usize = (1<<LOG2FFTSIZE);

fn main() {
    let mut xy         : [Complex<f64>; SIZE] = [Complex::new(0.0, 0.0); SIZE];
    let mut xy_out_fft : [Complex<f64>; SIZE] = [Complex::new(0.0, 0.0); SIZE];

    for i in 0..SIZE/2    { xy[i] = Complex::new(1.0, 0.0); }
    for i in SIZE/2..SIZE { xy[i] = Complex::new(-1.0, 0.0); }

// FFT
    let start_time = Instant::now();
    let ffto = fft::Fft::new();
    for _i in 0..FFT_REPEAT {
	ffto.fft(LOG2FFTSIZE, &mut xy_out_fft, &xy)
    }
    let elapsed_time = start_time.elapsed();
    let milliseconds = (elapsed_time.as_secs() as f64 * 1000.0) +
		       (elapsed_time.subsec_nanos() as f64 / 1_000_000.0);

    println!("{} piece(s) of {} pt FFT;    {} ms/piece\n\n", FFT_REPEAT, SIZE, milliseconds/FFT_REPEAT as f64);

    for i in 0..6 {
	println!("{}  {} {}", i, xy_out_fft[i].re, xy_out_fft[i].im);
    }
}
