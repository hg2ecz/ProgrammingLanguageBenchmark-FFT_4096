mod fft;

use std::time::Instant;

const LOG2FFTSIZE: u32 = 12;
const FFT_REPEAT: u32 = 1000;

const SIZE: usize = 1<<LOG2FFTSIZE;

fn main() {
    let mut xy         : [[f64; 2]; SIZE] = [[0.0; 2]; SIZE];
    let mut xy_out_fft : [[f64; 2]; SIZE] = [[0.0; 2]; SIZE];

    for item in xy.iter_mut().take(SIZE/2) { 
        *item = [ 1.0, 0.0];
    }
    
    for item in xy.iter_mut().take(SIZE).skip(SIZE/2) {
        *item = [-1.0, 0.0]; 
    }

    // FFT
    let start_time = Instant::now();

    let fourier = fft::Fourier::new();
    for _i in 0 .. FFT_REPEAT { 
        fourier.fft(LOG2FFTSIZE, &mut xy_out_fft, &xy)
    }
    
    let elapsed_time = start_time.elapsed();
    let milliseconds = (elapsed_time.as_secs() as f64 * 1000.0) +
                       (f64::from(elapsed_time.subsec_nanos()) / 1_000_000.0);

    println!("{} piece(s) of {} pt FFT;    {} ms/piece\n", FFT_REPEAT, SIZE, milliseconds/f64::from(FFT_REPEAT));

    for (i, item) in xy_out_fft.iter().enumerate().take(6) {
        println!("{}\t({}; {})", i, item[0], item[1]);
    }
}
