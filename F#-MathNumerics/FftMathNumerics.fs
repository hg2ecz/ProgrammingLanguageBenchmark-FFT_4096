module FftMathNumerics

open System.Diagnostics
open System.Numerics
open MathNet
open MathNet.Numerics

module FftModule =

    let compareSequences seq1 seq2 = 
        // Compare the first 5 decimal digit of the double
        Seq.compareWith (fun x y -> int ( x * 100000.0) - int (y * 100000.0)) seq1 seq2
        |> (=) 0

    [<EntryPoint>]
    let main argv =

        let LOG2FFTSIZE = 16
        let FFT_REPEAT = 1000

        let SIZE = 1 <<< LOG2FFTSIZE

        let input = Array.init SIZE (fun _ -> Complex())

        for i = 0 to SIZE/2 - 1 do
            input.[i] <- Complex(1.0, 0.0)

        for i = SIZE/2 to SIZE - 1 do
            input.[i] <- Complex(-1.0, 0.0)

        Control.UseManaged();
        printfn "Linear Algebra Provider = %A" Control.LinearAlgebraProvider

        let stopwatch = Stopwatch.StartNew()

        for i = 0 to FFT_REPEAT do
            let transformed = Array.copy input
            IntegralTransforms.Fourier.Forward transformed

        let elapsed1 = stopwatch.Elapsed
        printfn "Elapsed1: %A ms" (elapsed1.TotalMilliseconds / float FFT_REPEAT)

        // Using the Intel MKL native provider
        Control.UseNativeMKL()
        printfn "Linear Algebra Provider = %A" Control.LinearAlgebraProvider

        stopwatch.Restart()

        for i = 0 to FFT_REPEAT do
            let transformed' = Array.copy input
            IntegralTransforms.Fourier.Forward transformed'

        let elapsed2 = stopwatch.Elapsed
        printfn "Elapsed1: %A ms" (elapsed2.TotalMilliseconds / float FFT_REPEAT)

        // printfn "Solution1 equals to Solution2 = %A" (compareSequences transformed transformed')
        printfn "Elapsed1 / Elapsed2 = %A" (elapsed1.TotalMilliseconds / elapsed2.TotalMilliseconds)
        0 // return an integer exit code
