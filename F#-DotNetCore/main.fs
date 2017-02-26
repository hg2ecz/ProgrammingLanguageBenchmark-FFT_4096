namespace FftPerformanceDemo

module MainModule =

    open System.Numerics
    open System.Diagnostics

    open FftModule

    [<EntryPoint>]
    let main argv =

        let LOG2FFTSIZE = 12
        let FFT_REPEAT = 100000

        let SIZE = 1 <<< LOG2FFTSIZE

        let xy = Array.init SIZE (fun i -> if i < SIZE/2 then Complex.One else -Complex.One)

        for i = 0 to FFT_REPEAT - 1 do
            let fft_out = FftModule.fft LOG2FFTSIZE xy

            // Changed this because of different scoping in Python and F#
            if i = 0 then
                for i = 0 to 6 do
                    printfn "%i %A" i fft_out.[i]

        let stopwatch = Stopwatch.StartNew()

        for i = 0 to FFT_REPEAT - 1 do
            FftModule.fft LOG2FFTSIZE xy |> ignore

        let eltime = stopwatch.ElapsedMilliseconds
        printfn "%6d piece of %d pt FFT;  %9.5f ms/piece\n" FFT_REPEAT  (1 <<< LOG2FFTSIZE) (float eltime / float FFT_REPEAT)

        0
