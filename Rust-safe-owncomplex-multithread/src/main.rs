mod fft;

use std::time::Instant;
use std::thread;

const LOG2FFTSIZE: u32 = 12;
const FFT_REPEAT: u32 = 1000;

const SIZE: usize = (1<<LOG2FFTSIZE);
const MAXTHREAD: usize = 64;

fn main() {
    let mut xy         : [[fft::Cplx; SIZE]; MAXTHREAD] = [[fft::Cplx {re: 1.0, im: 0.0}; SIZE]; MAXTHREAD];
    let mut xy_out_fft : [[fft::Cplx; SIZE]; MAXTHREAD] = [[fft::Cplx {re: 1.0, im: 0.0}; SIZE]; MAXTHREAD];

    for j in 0..MAXTHREAD {
        for i in 0..SIZE/2    { xy[j][i].re =  1.0; xy[j][i].im = 0.0; }
        for i in SIZE/2..SIZE { xy[j][i].re = -1.0; xy[j][i].im = 0.0; }
    }

// FFT
    let start_time = Instant::now();

    let num_cores = 4;
    let mut children = vec![];

    for tid in 0..num_cores {
        let xy_slice: [fft::Cplx; SIZE] = xy[tid as usize].clone();

        children.push(thread::spawn(move || -> (u32, [fft::Cplx; SIZE]) {
            let mut xy_out_slice : [fft::Cplx; SIZE] = [fft::Cplx {re: 1.0, im: 0.0}; SIZE];
            let f = fft::Fft::new();
            for _i in 0..FFT_REPEAT/num_cores {
                f.fft(LOG2FFTSIZE, &mut xy_out_slice, &xy_slice);
            }
            println!("bent: {} {}", xy_out_slice[1].re, xy_out_slice[1].im);
            return (tid, xy_out_slice);
        })); // push
    }

    for child in children {
        let (tid, xyout) = child.join().unwrap();
        println!("join: tid: {} re: {} im: {}", tid, xyout[1].re, xyout[1].im);

        for i in 0..SIZE {
            xy_out_fft[tid as usize][i].re = xyout[i].re;
            xy_out_fft[tid as usize][i].im = xyout[i].im;
        }
        //println!("{} {}", xy_out_fft[0][1].re, xy_out_fft[0][1].im);
    }

    let elapsed_time = start_time.elapsed();
    let milliseconds = (elapsed_time.as_secs() as f64 * 1000.0) +
		       (elapsed_time.subsec_nanos() as f64 / 1_000_000.0);

    println!("{} piece(s) of {} pt FFT;    {} ms/piece\n\n", FFT_REPEAT, SIZE, milliseconds/FFT_REPEAT as f64);

    println!("{}", xy_out_fft[0].len());
//    println!("{} {}", xy_out_fft[0][0].re, xy_out_fft[0][0].im);
    
    for i in 0..6 {
//	println!("{}  {} {}", i, xy_out_fft[0][i].re, xy_out_fft[0][i].im);
    }
    println!("");
    for i in 0..6 {
//	println!("{}  {} {}", i, xy_out_fft[1][i].re, xy_out_fft[1][i].im);
    }
}
