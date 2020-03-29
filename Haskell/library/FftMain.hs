-- | Main module for FFT benchmarking
module FftMain (main) where

import Control.Monad
import Data.Complex
import Data.Bits
import Data.Time
import Data.Word

import qualified Fft (fft)
import qualified Data.Vector.Unboxed.Mutable as MVector

main :: IO ()
main = do
    start <- getCurrentTime

    results <- forM [1 .. fftRepeat] $ \_ -> Fft.fft log2FftSize xy
    let result = head results

    firstResults <- forM [0 .. 6] $ \i -> MVector.read result i
    putStrLn $ concatMap formatResult $ zip [0..] firstResults

    end <- getCurrentTime
    let totalTime = diffUTCTime end start

    putStrLn $ "Total time: " ++ (show totalTime)
    putStrLn $ "Total iterarions: " ++ (show fftRepeat)

    where
        xy = [1.0 :+ 0.0 | _ <- [0 .. (size `div` 2) - 1]] <>
             [(-1.0) :+ 0.0 | _ <- [size `div` 2 .. size - 1]]

        log2FftSize :: Word64
        log2FftSize = 12

        fftRepeat :: Integer
        fftRepeat = 1000

        size :: Word64
        size = 1 `shift` (fromIntegral log2FftSize)

        formatResult :: (Int, Complex Double) -> String
        formatResult r = (show $ fst r) ++ " -> " ++ (show $ snd r) ++ "\n"
