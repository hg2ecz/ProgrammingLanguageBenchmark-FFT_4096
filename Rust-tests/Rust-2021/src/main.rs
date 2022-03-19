mod fft;

use num_complex::Complex;
use std::time::Instant;

const LOG2FFTSIZE: u32 = 12;
const FFT_REPEAT: u32 = 10000;
const SIZE: usize = 1 << LOG2FFTSIZE;

fn main() {
    let mut xy = [Complex::new(0.0, 0.0); SIZE];
    let mut xy_out_fft = [Complex::new(0.0, 0.0); SIZE];

    for xy_act in xy.iter_mut().take(SIZE / 2) {
        *xy_act = Complex::new(1.0, 0.0);
    }
    for xy_act in xy.iter_mut().take(SIZE).skip(SIZE / 2) {
        *xy_act = Complex::new(-1.0, 0.0);
    }

    // FFT
    let start_time = Instant::now();
    let ffto = fft::Fft::new();
    for _i in 0..FFT_REPEAT {
        ffto.fft(LOG2FFTSIZE, &mut xy_out_fft, &xy)
    }
    let elapsed_time = start_time.elapsed();
    let milliseconds = (elapsed_time.as_secs() as f64 * 1000.0) + (elapsed_time.subsec_nanos() as f64 / 1_000_000.0);

    println!(
        "{} piece(s) of {} pt FFT;    {} ms/piece\n\n",
        FFT_REPEAT,
        SIZE,
        milliseconds / FFT_REPEAT as f64
    );

    for (i, &xy_act) in xy_out_fft.iter().enumerate().take(6) {
        println!("{}  {} {}", i, xy_act.re, xy_act.im);
    }
}
