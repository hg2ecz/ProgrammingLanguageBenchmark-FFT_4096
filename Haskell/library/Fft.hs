module Fft (fft) where

import Data.Bits
import Data.Complex
import Data.Word
import Control.Monad
import Control.Monad.Primitive (PrimState)

import qualified Data.Vector.Unboxed.Mutable as MVector

-- | The main FFT function.
fft :: Int -> [Complex Double] -> IO (MVector.MVector (PrimState IO) (Complex Double))
fft log2point input = do
    let size = length input
    output <- MVector.new size

    forM_ [0 .. size - 1] $ \i -> do
        let brev0 = fromIntegral i :: Word32

        let brev1 = ((brev0 .&. 0xaaaaaaaa) `shift` (-1)) .|. ((brev0 .&. 0x55555555) `shift` 1)
        let brev2 = ((brev1 .&. 0xcccccccc) `shift` (-2)) .|. ((brev1 .&. 0x33333333) `shift` 2)
        let brev3 = ((brev2 .&. 0xf0f0f0f0) `shift` (-4)) .|. ((brev2 .&. 0x0f0f0f0f) `shift` 4)
        let brev4 = ((brev3 .&. 0xff00ff00) `shift` (-8)) .|. ((brev3 .&. 0x00ff00ff) `shift` 8)
        let brev5 = (brev4 `shift` (-16)) .|. (brev4 `shift` 16)

        let brev6 = brev5 `shift` (-(32 - log2point))

        MVector.write output (fromIntegral brev6) $ input !! i

    forM_ [0 .. log2point - 1] $ \l2pt -> do
        let wphase_xy = phasevec !! l2pt
        let mmax = 1 `shift` l2pt

        w_xy <- MVector.new 1
        MVector.write w_xy 0 $ 1.0 :+ 0.0

        forM_ [0 .. mmax - 1] $ \m -> do
            let f = mmax `shift` 1

            forM_ [m + f * x | x <- [0 .. ((size - m - 1) `div` f)]] $ \i -> do
                temp1 <- MVector.read w_xy 0
                temp2 <- MVector.read output $ i + mmax
                let temp = temp1 * temp2

                out1 <- MVector.read output $ i
                MVector.write output (i + mmax) $ out1 - temp

                out2 <- MVector.read output i
                MVector.write output i $ out2 + temp

            w_xy1 <- MVector.read w_xy 0
            MVector.write w_xy 0 $ w_xy1 * wphase_xy

    return output

    where
        points = [2 `shift` x | x <- [0 .. 31]] :: [Int]

        phasevec = [cos (-2.0 * pi / fromIntegral x) :+
            sin (-2.0 * pi / fromIntegral x) | x <- points]
