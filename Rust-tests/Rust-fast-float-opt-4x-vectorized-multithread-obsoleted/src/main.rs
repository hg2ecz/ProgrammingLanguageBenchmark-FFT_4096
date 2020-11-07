//mod fft;        // -- if we used as module
extern crate fft; // -- if we used as lib 
extern crate num_cpus;

use std::time::Instant;
use std::thread;

const LOG2FFTSIZE: u32 = 12;
const FFT_REPEAT: u32 = 100_000;
const SIZE: usize = (1<<LOG2FFTSIZE);

fn main() {
    let max_threads = num_cpus::get();

    let mut xy         = (vec![0f32; SIZE], vec![0f32; SIZE]); // vectoization-friendly
    let xy_out_fft = (vec![0f32; SIZE], vec![0f32; SIZE]);

    //for i in 0..SIZE/2    { xy.0[i] = 1.0; xy.1[i] = 0.0; }
    //for i in SIZE/2..SIZE { xy.0[i] = -1.0; xy.1[i] = 0.0; }
    for i in 0..SIZE { xy.0[i] = 1.+i as f32/1000.; xy.1[i]= i as f32/2000.; }

    let mut thvec = Vec::new();
    let mut thvec_out = Vec::new();
    for _i in 0..max_threads {
	thvec.push(xy.clone());
	thvec_out.push(xy_out_fft.clone());
    }

// FFT
    let start_time = Instant::now();
// -- multithread --
//    let ffto = fft::Fft::new();
//    for _i in 0..FFT_REPEAT {
//	xy_out_fft = ffto.fft(LOG2FFTSIZE, &xy);
//    }
    let mut children = Vec::new();

    for tid in 0..max_threads {
	let xy_mt: (Vec<f32>, Vec<f32>) = thvec[tid].clone();
        children.push(thread::spawn(move || -> (u32, (Vec<f32>, Vec<f32>)) {
	    let mut xy_out_fft_mt = (Vec::new(), Vec::new());
	    let f = fft::Fft::new();
	    for _i in 0..FFT_REPEAT / max_threads as u32 {
		xy_out_fft_mt = f.fft(LOG2FFTSIZE, &xy_mt);
	    }
	    return (tid as u32, xy_out_fft_mt);
	})); // push
    }

    for child in children {
	let (tid,  xyout) = child.join().unwrap();
	thvec_out[tid as usize] = xyout;
    }
// -- end of multithread --

    let elapsed_time = start_time.elapsed();
    let milliseconds = (elapsed_time.as_secs() as f64 * 1000.0) +
		       (elapsed_time.subsec_nanos() as f64 / 1_000_000.0);

    println!("{} piece(s) of {} pt FFT;    {} ms/piece\n\n", FFT_REPEAT, SIZE, milliseconds/FFT_REPEAT as f64);

    for i in 0..6 {
	println!("{}  {} {}", i, thvec_out[0].0[i], thvec_out[0].1[i]);
    }
}
