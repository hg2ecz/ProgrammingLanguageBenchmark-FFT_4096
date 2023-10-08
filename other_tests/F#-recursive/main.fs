open System
open System.Numerics
open System.Diagnostics

let rec fft_recursive (buf: Complex array) (out: Complex array) (n: int) (step: int) (shift: int) =
    if step < n then
        fft_recursive out buf n (step * 2) shift
        fft_recursive out buf n (step * 2) (shift + step) 
 
        let phase = Complex(1.0, 0.0)
        let phasediff = Complex.FromPolarCoordinates(1.0, (2.0 * Math.PI  * (float step) / (float n)))

        let rec loop i phase =
            if i < n then
                let t = phase * out.[shift + i + step]
                buf.[shift + i / 2] <- out.[shift + i] + t
                buf.[shift + (i + n)/2] <- out.[shift + i] - t

                loop (i + 2 * step) (phase * phasediff)

        loop 0 phase

[<EntryPoint>]
let main args =
    let LOG2FFTSIZE = 12
    let SIZE = 1 <<< LOG2FFTSIZE
    let FFT_REPEAT = 1000

    let xy = Array.init SIZE (fun i -> 
        if i < SIZE / 2 
            then Complex.One
            else -Complex.One
        )

    let sw = Stopwatch.StartNew()

    for i = 0 to FFT_REPEAT - 1 do
        let xy_out_fft = Array.copy xy
        let xy_tmp = Array.init SIZE (fun _ -> Complex.Zero)
        fft_recursive xy_out_fft xy_tmp SIZE 1 0

        if i = 0 then
            printfn "%A" xy_out_fft

    printfn "%6d piece(s) of %d pt FFT;  %9.5f ms/piece" FFT_REPEAT SIZE (float sw.ElapsedMilliseconds / float FFT_REPEAT)

    0