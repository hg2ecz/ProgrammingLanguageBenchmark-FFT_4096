mod fft;
mod fft_algebraic;
mod fft_c;

use num_complex::{Complex, ComplexFloat};
use rand::Rng;
use std::time::Instant;

type Cf32 = fft::Cf32;
const LOG2FFTSIZE: u32 = 12;
const FFT_REPEAT: u32 = 10_000;
const SIZE: usize = 1 << LOG2FFTSIZE;

fn stop_time(start_time: &Instant, xy_out_fft: &[Cf32], txt: &str) {
    let elapsed_time = start_time.elapsed();
    let milliseconds = (elapsed_time.as_secs() as f64 * 1000.0)
        + (elapsed_time.subsec_nanos() as f64 / 1_000_000.0);

    println!(
        "\n{txt} {FFT_REPEAT} piece(s) of {SIZE} pt FFT;    {:.5} ms/piece",
        milliseconds / FFT_REPEAT as f64
    );

    println!("bin        real             imag           absval");
    for (i, &xy_act) in xy_out_fft.iter().enumerate().take(6) {
        println!(
            "{i:3} {:16.4} {:16.4} {:16.4}",
            xy_act.re,
            xy_act.im,
            xy_act.abs()
        );
    }
}

fn main() {
    let mut xy = [Complex::new(0.0, 0.0); SIZE];
    let mut xy_out_fft = [Complex::new(0.0, 0.0); SIZE];

    for xy_act in xy.iter_mut().take(SIZE / 2) {
        *xy_act = Complex::new(1.0, 0.0);
    }
    for xy_act in xy.iter_mut().take(SIZE).skip(SIZE / 2) {
        *xy_act = Complex::new(-1.0, 0.0);
    }

    // FFT - Rust
    let ffto = fft::Fft::new();
    let start_time = Instant::now();
    for _i in 0..FFT_REPEAT {
        ffto.fft(&mut xy_out_fft, &xy)
    }
    stop_time(&start_time, &xy_out_fft, "Rust code (+1.0, -1.0)");

    // Random number
    let mut rng = rand::thread_rng();
    for xy_act in xy.iter_mut().take(SIZE / 2) {
        *xy_act = Complex::new(rng.gen_range(-0.707..0.707), rng.gen_range(-0.707..0.707));
    }
    for xy_act in xy.iter_mut().take(SIZE).skip(SIZE / 2) {
        *xy_act = Complex::new(rng.gen_range(-0.707..0.707), rng.gen_range(-0.707..0.707));
    }

    // FFT - Rust
    let ffto = fft::Fft::new();
    let start_time = Instant::now();
    for _i in 0..FFT_REPEAT {
        ffto.fft(&mut xy_out_fft, &xy)
    }
    stop_time(&start_time, &xy_out_fft, "Rust code (rndnum)");

    // FFT - C
    let start_time = Instant::now();
    for _i in 0..FFT_REPEAT {
        fft_c::fftc(&mut xy_out_fft, &xy)
    }
    stop_time(&start_time, &xy_out_fft, "C module (normal)");

    // FFT - C unsafe
    let start_time = Instant::now();
    for _i in 0..FFT_REPEAT {
        fft_c::fftcu(&mut xy_out_fft, &xy)
    }
    stop_time(&start_time, &xy_out_fft, "C module (unsafe)");

    // FFT - Rust
    let ffto = fft_algebraic::Fft::new();
    let start_time = Instant::now();
    for _i in 0..FFT_REPEAT {
        ffto.fft(&mut xy_out_fft, &xy)
    }
    stop_time(&start_time, &xy_out_fft, "Rust algebraic code (+1.0, -1.0)");
}
