mod fft;

use std::time::Instant;

const LOG2FFTSIZE: u32 = 12;
const FFT_REPEAT: u32 = 1000;

const SIZE: usize = (1<<LOG2FFTSIZE);

fn main() {
    let mut xy         : [fft::Cplx; SIZE] = [fft::Cplx {re: 1.0, im: 0.0}; SIZE];
    let mut xy_out_fft : [fft::Cplx; SIZE] = [fft::Cplx {re: 1.0, im: 0.0}; SIZE];

    for i in 0..SIZE/2    { xy[i].re =  1.0; xy[i].im = 0.0; }
    for i in SIZE/2..SIZE { xy[i].re = -1.0; xy[i].im = 0.0; }

// FFT
    let start_time = Instant::now();
    let f = fft::Fft::new();
    for _i in 0..FFT_REPEAT {
	f.fft(LOG2FFTSIZE, &mut xy_out_fft, &xy);
    }
    let elapsed_time = start_time.elapsed();
    let milliseconds = (elapsed_time.as_secs() as f64 * 1000.0) +
		       (elapsed_time.subsec_nanos() as f64 / 1_000_000.0);

    println!("{} piece(s) of {} pt FFT;    {} ms/piece\n\n", FFT_REPEAT, SIZE, milliseconds/FFT_REPEAT as f64);

    for i in 0..6 {
	println!("{}  {} {}", i, xy_out_fft[i].re, xy_out_fft[i].im);
    }
}
