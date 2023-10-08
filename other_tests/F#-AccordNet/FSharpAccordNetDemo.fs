namespace FSharpAccordNetDemo

module MainModule =
    open System.Numerics
    open System.Diagnostics
    open Accord.Math

    [<EntryPoint>]
    let main argv =

        let LOG2FFTSIZE = 12
        let SIZE = 1 <<< LOG2FFTSIZE
        let FFT_REPEAT = 1000

        let input = Array.init SIZE (fun _ -> Complex())

        for i = 0 to SIZE/2 - 1 do
            input.[i] <- Complex(1.0, 0.0)

        for i = SIZE/2 to SIZE - 1 do
            input.[i] <- Complex(-1.0, 0.0)

        let stopwatch = Stopwatch.StartNew()

        for i = 0 to FFT_REPEAT do
            let transformed = Array.copy input
            FourierTransform.FFT (transformed, Accord.Math.FourierTransform.Direction.Forward) 

        let elapsed1 = stopwatch.Elapsed
        printfn "Elapsed time: %A ms" (elapsed1.TotalMilliseconds / float FFT_REPEAT)
        
        0 // return an integer exit code
