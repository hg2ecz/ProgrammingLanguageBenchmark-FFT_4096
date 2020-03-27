module Fft (fft) where

import Data.Bits
import Data.Complex
import Control.Monad
import Control.Monad.Primitive (PrimState)

import qualified Data.Vector.Unboxed.Mutable as MVector

-- | The main FFT function.
fft :: Int -> [Complex Float] -> IO (MVector.MVector (PrimState IO) (Complex Float))
fft log2point input = do
    let size = length input
    output <- MVector.new size

    forM_ [0 .. 1 `shift` log2point - 1] $ \i -> do
        let brev0 = i

        let brev1 = ((brev0 .&. 0xaaaaaaaa) `shift` (-1)) .|. ((brev0 .&. 0x55555555) `shift` 1)
        let brev2 = ((brev1 .&. 0xcccccccc) `shift` (-2)) .|. ((brev1 .&. 0x33333333) `shift` 2)
        let brev3 = ((brev2 .&. 0xf0f0f0f0) `shift` (-4)) .|. ((brev2 .&. 0x0f0f0f0f) `shift` 4)
        let brev4 = ((brev3 .&. 0xff00ff00) `shift` (-8)) .|. ((brev3 .&. 0x00ff00ff) `shift` 8)
        let brev5 = (brev4 `shift` (-16)) .|. (brev4 `shift` 16)

        let brev6 = brev5 `shift` (-32 + log2point);

        MVector.write output brev6 $ input !! i

    return output

    where
        points = [2 `shift` x | x <- [0 .. 32]] :: [Int]
        phasevec = [cos (-2.0 * pi / fromIntegral x) :+
            sin (-2.0 * pi / fromIntegral x) | x <- points]
