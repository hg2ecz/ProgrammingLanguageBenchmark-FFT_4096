module FftMathNumerics

open System.Diagnostics
open System.Numerics
open MathNet
open MathNet.Numerics

module FftModule =

    let FFT_REPEAT = 1000

    type Provicers =
    | Managed
    | MultiThreaded
    | NativeMKL
    | NativeCUDA

    let compareSequences seq1 seq2 = 
        // Compare the first 5 decimal digit of the double
        Seq.compareWith (fun x y -> int ( x * 100000.0) - int (y * 100000.0)) seq1 seq2
        |> (=) 0

    let switchProvider provider =
        match provider with
        | Managed -> Control.UseManaged()
        | MultiThreaded -> Control.UseMultiThreading()
        | NativeMKL -> Control.UseNativeMKL()
        | NativeCUDA -> Control.UseNativeCUDA()
        | _ -> ()

        printfn "Linear Algebra Provider = %A" Control.LinearAlgebraProvider

    let fft =
        IntegralTransforms.Fourier.Forward

    let bluesteinFft input =
        let options = IntegralTransforms.FourierOptions ()
        IntegralTransforms.Fourier.BluesteinForward (input, options)

    let radix2ForwardFft input =
        let options = IntegralTransforms.FourierOptions ()
        IntegralTransforms.Fourier.Radix2Forward (input, options)

    [<EntryPoint>]
    let main argv =

        let LOG2FFTSIZE = 12

        let SIZE = 1 <<< LOG2FFTSIZE

        let input = Array.init SIZE (fun _ -> Complex())

        for i = 0 to SIZE/2 - 1 do
            input.[i] <- Complex(1.0, 0.0)

        for i = SIZE/2 to SIZE - 1 do
            input.[i] <- Complex(-1.0, 0.0)

        let providers = [
            Managed
            MultiThreaded
            NativeMKL
            NativeCUDA
        ]

        let algorithms = [
            ("FFT", fft)
            ("Bluestein", bluesteinFft)
            ("Radix-2", radix2ForwardFft)
        ]

        for provider in providers do
            for algorithm in algorithms do
                let stopwatch = Stopwatch.StartNew()

                for i = 0 to FFT_REPEAT do
                    let transformed = Array.copy input
                    (snd algorithm) transformed

                let elapsed1 = stopwatch.Elapsed
                printfn "%A %A: %A ms" provider (fst algorithm) (elapsed1.TotalMilliseconds / float FFT_REPEAT)

        0 // return an integer exit code
