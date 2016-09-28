namespace FftPerformanceDemo

module FftModule =
    open System
    open System.Numerics

    let mutable phasevec_exist = false
    let mutable phasevec = Array.init 32 (fun _ -> Complex())

    let fft log2point (xy_in: Complex array) =
        if not phasevec_exist then
            for i = 0 to 32 - 1 do
                let point = 2 <<< i
                phasevec.[i] <- Complex(Math.Cos(-2.0 * Math.PI / float point), Math.Sin(-2.0 * Math.PI / float point))
            phasevec_exist <- true

        let mutable xy_out = Array.init (1 <<< log2point) (fun _ -> Complex())

        for i = 0 to (1 <<< log2point) - 1 do
            let mutable (brev: uint32) = uint32 i
            brev <- ((brev &&& uint32 0xaaaaaaaa) >>> 1) ||| ((brev &&& uint32 0x55555555) <<< 1)
            brev <- ((brev &&& uint32 0xcccccccc) >>> 2) ||| ((brev &&& uint32 0x33333333) <<< 2)
            brev <- ((brev &&& uint32 0xf0f0f0f0) >>> 4) ||| ((brev &&& uint32 0x0f0f0f0f) <<< 4)
            brev <- ((brev &&& uint32 0xff00ff00) >>> 8) ||| ((brev &&& uint32 0x00ff00ff) <<< 8)
            brev <- (brev >>> 16) ||| (brev <<< 16)

            brev <- brev >>> (32 - log2point)
            xy_out.[int brev] <- xy_in.[i]

        // here begins the Danielson-Lanczos section
        let n = 1 <<< log2point
        let mutable l2pt = 0
        let mutable mmax = 1

        while n > mmax do
            let istep = mmax <<< 1

            let wphase_XY = phasevec.[l2pt]
            l2pt <- l2pt + 1

            let mutable w_XY = Complex(1.0, 0.0)

            for m = 0 to mmax - 1 do
                // FIXME: slow, profile this: [m..x..n]    
                for i in [m .. istep .. n - 1] do
                    let tempXY = w_XY * xy_out.[i + mmax]
                    xy_out.[i+mmax] <- xy_out.[i] - tempXY
                    xy_out.[i] <- xy_out.[i] + tempXY

                w_XY <- w_XY * wphase_XY // rotate

            mmax <- istep

        xy_out

