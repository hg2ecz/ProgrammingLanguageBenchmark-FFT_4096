//mod fft;        // -- if we used as module
extern crate fft; // -- if we used as lib 

use std::time::Instant;

const LOG2FFTSIZE: u32 = 12;
const FFT_REPEAT: u32 = 10000;
const SIZE: usize = (1<<LOG2FFTSIZE);

fn main() {
    let mut xy         = (vec![0f32; SIZE], vec![0f32; SIZE]); // vectoization-friendly
    let mut xy_out_fft = (vec![0f32; SIZE], vec![0f32; SIZE]);

    //for i in 0..SIZE/2    { xy.0[i] = 1.0; xy.1[i] = 0.0; }
    //for i in SIZE/2..SIZE { xy.0[i] = -1.0; xy.1[i] = 0.0; }
    for i in 0..SIZE { xy.0[i] = 1.+i as f32/1000.; xy.1[i]= i as f32/2000.; }


// FFT
    let start_time = Instant::now();
    let ffto = fft::Fft::new();
    for _i in 0..FFT_REPEAT {
	xy_out_fft = ffto.fft(LOG2FFTSIZE, &xy);
    }
    let elapsed_time = start_time.elapsed();
    let milliseconds = (elapsed_time.as_secs() as f64 * 1000.0) +
		       (elapsed_time.subsec_nanos() as f64 / 1_000_000.0);

    println!("{} piece(s) of {} pt FFT;    {} ms/piece\n\n", FFT_REPEAT, SIZE, milliseconds/FFT_REPEAT as f64);

    for i in 0..6 {
	println!("{}  {} {}", i, xy_out_fft.0[i], xy_out_fft.1[i]);
    }
}
