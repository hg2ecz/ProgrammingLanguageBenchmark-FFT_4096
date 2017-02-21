mod fft;

use std::time::Instant;

const LOG2FFTSIZE: u32 = 12;
const FFT_REPEAT: u32 = 1000;

const SIZE: usize = (1<<LOG2FFTSIZE);
static mut XY         : [[f64; 2]; SIZE] = [[0.0; 2]; SIZE];
static mut XY_OUT_FFT : [[f64; 2]; SIZE] = [[0.0; 2]; SIZE];

fn main() {

    unsafe {
      for i in 0..SIZE/2    { XY[i] = [ 1.0, 0.0]; }
      for i in SIZE/2..SIZE { XY[i] = [-1.0, 0.0]; }
    }

// FFT
    let start_time = Instant::now();
    for _i in 0..FFT_REPEAT { unsafe {  fft::fft(LOG2FFTSIZE, &mut XY_OUT_FFT, &XY) } }
    let elapsed_time = start_time.elapsed();
    let milliseconds = (elapsed_time.as_secs() as f64 * 1000.0) +
		       (elapsed_time.subsec_nanos() as f64 / 1_000_000.0);

    println!("{} piece(s) of {} pt FFT;    {} ms/piece\n\n", FFT_REPEAT, SIZE, milliseconds/FFT_REPEAT as f64);

    for i in 0..6 {
	unsafe { println!("{}  {} {}", i, XY_OUT_FFT[i][0], XY_OUT_FFT[i][1]) }
    }
}
