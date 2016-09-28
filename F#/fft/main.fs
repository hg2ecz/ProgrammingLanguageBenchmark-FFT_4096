namespace FftPerformanceDemo

module MainModule =

    open System.Numerics
    open System.Diagnostics

    open FftModule

    [<EntryPoint>]
    let main argv =

        let LOG2FFTSIZE = 16
        let FFT_REPEAT = 10

        let SIZE = 1 <<< LOG2FFTSIZE

        let xy = Array.init SIZE (fun _ -> Complex())

        for i = 0 to SIZE/2 - 1 do
            xy.[i] <- Complex(1.0, 0.0)

        for i = SIZE/2 to SIZE - 1 do
            xy.[i] <- Complex(-1.0, 0.0)

        let stopwatch = Stopwatch.StartNew()

        for i = 0 to FFT_REPEAT - 1 do
            let fft_out = FftModule.fft 16 xy
            let eltime = stopwatch.ElapsedMilliseconds
            printfn "%6d piece of %d pt FFT;  %9.5f ms/piece\n" FFT_REPEAT  (1 <<< LOG2FFTSIZE) (float eltime / float FFT_REPEAT)

            // Changed this because of different scoping in Python and F#
            if i = 0 then
                for i = 0 to 6 do
                    printfn "%i %A" i fft_out.[i]

        0
