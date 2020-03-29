-- | Main module for FFT benchmarking
module Main (main) where

import Control.Monad
import Data.Complex
import Data.Bits
import Data.Time
import Control.Concurrent.Async

import qualified Fft (fft)
import qualified Data.Vector.Unboxed.Mutable as MVector

main :: IO ()
main = do
    start <- getCurrentTime

    results <- forConcurrently [1 .. fftRepeat] $ \_ -> Fft.fft log2FftSize xy
    let result = head results

    firstResults <- forM [0 .. 6] $ \i -> MVector.read result i
    putStrLn $ concatMap formatResult $ zip [0..] firstResults

    end <- getCurrentTime
    let totalTime = diffUTCTime end start

    putStrLn $ "Total time: " ++ (show totalTime)
    putStrLn $ "Total iterations: " ++ (show fftRepeat)

    where
        xy = [1.0 :+ 0.0 | _ <- [0 .. (size `div` 2) - 1]] <>
             [(-1.0) :+ 0.0 | _ <- [size `div` 2 .. size - 1]]

        log2FftSize :: Int
        log2FftSize = 12

        fftRepeat :: Integer
        fftRepeat = 10000

        size :: Int
        size = 1 `shift` log2FftSize

        formatResult :: (Int, Complex Double) -> String
        formatResult r = (show $ fst r) ++ " -> " ++ (show $ snd r) ++ "\n"
