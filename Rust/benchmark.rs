mod fft;

use std::mem;
use std::time::Instant;

const LOG2FFTSIZE: u32 = 12;
const FFT_REPEAT: u32 = 1000;

const SIZE: usize = (1<<LOG2FFTSIZE);
static xy: [[f64; 2]; SIZE] = std::mem::uninitialized();
static xy_out_fft: [[f64; 2]; SIZE] = std::mem::uninitialized();

fn main() {
    let i: i32;
    for i in 0..SIZE/2    { xy[i] = [ 1.0, 0.0]; }
    for i in SIZE/2..SIZE { xy[i] = [-1.0, 0.0]; }

// FFT
    let startTime = Instant::now();
    for i in 0..FFT_REPEAT { fft::fft(LOG2FFTSIZE, xy_out_fft, xy) }
    let elapsedTime = startTime.elapsed();
    let milliseconds = (elapsedTime.as_secs() as f64 * 1000.0) +
		       (elapsedTime.subsec_nanos() as f64 / 1_000_000.0);

    println!("{} piece(s) of {} pt FFT;    {} ms/piece\n\n", FFT_REPEAT, SIZE, milliseconds/FFT_REPEAT as f64);

    for i in 0..6 {
	println!("{}  {} {}", i, xy_out_fft[i][0], xy_out_fft[i][1])
    }
}
