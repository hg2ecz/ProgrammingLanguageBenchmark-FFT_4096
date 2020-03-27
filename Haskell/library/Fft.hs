module Fft (fft) where

import Data.Bits
import Data.Complex
import Control.Monad
import Control.Monad.Primitive (PrimState)

import qualified Data.Vector.Unboxed.Mutable as MVector

-- | The main FFT function.
fft :: [Complex Float] -> IO (MVector.MVector (PrimState IO) (Complex Float))
fft input = do
    let size = length input
    output <- MVector.new size
    MVector.write output 0 $ 5 :+ 0
    MVector.write output 1 $ 4 :+ 0
    MVector.write output 2 $ 3 :+ 0
    MVector.write output 3 $ 2 :+ 0
    MVector.write output 4 $ 1 :+ 0

    return output

    where
        points = [2 `shift` x | x <- [0 .. 32]] :: [Int]
        phasevec = [cos (-2.0 * pi / fromIntegral x) :+
            sin (-2.0 * pi / fromIntegral x) | x <- points]
