mod fft;

use std::time::Instant;

const LOG2FFTSIZE: u32 = 12;
const FFT_REPEAT: u32 = 10000;

const SIZE: usize = (1 << LOG2FFTSIZE);

fn main() {
    let mut xy: [fft::Cplx<f64>; SIZE] = [fft::Cplx { re: 1.0, im: 0.0 }; SIZE];
    let mut xy_out_fft: [fft::Cplx<f64>; SIZE] = [fft::Cplx { re: 1.0, im: 0.0 }; SIZE];

    for item in xy.iter_mut().take(SIZE / 2) {
        item.re = 1.0;
        item.im = 0.0;
    }
    for item in xy.iter_mut().take(SIZE).skip(SIZE / 2) {
        item.re = -1.0;
        item.im = 0.0;
    }

    // FFT
    let start_time = Instant::now();
    let f = fft::Fft::new();
    for _i in 0..FFT_REPEAT {
        f.fft(LOG2FFTSIZE, &mut xy_out_fft, &xy);
    }
    let elapsed_time = start_time.elapsed();
    let milliseconds = (elapsed_time.as_secs() as f64 * 1000.0)
        + (elapsed_time.subsec_nanos() as f64 / 1_000_000.0);

    println!(
        "{} piece(s) of {} pt FFT;    {} ms/piece\n\n",
        FFT_REPEAT,
        SIZE,
        milliseconds / FFT_REPEAT as f64
    );

    for (i, item) in xy_out_fft.iter().enumerate().take(6) {
        println!("{}  {} {}", i, item.re, item.im);
    }
}
