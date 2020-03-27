-- | Main module for FFT benchmarking
module FftMain (main) where

import Control.Monad
import Data.Complex
import Data.Bits
import Data.Time

import qualified Fft (fft)
import qualified Data.Vector.Unboxed.Mutable as MVector

main :: IO ()
main = do
    start <- getCurrentTime

    results <- forM [1 .. fftRepeat] $ \_ -> Fft.fft log2FftSize xy
    let result = head results

    firstResults <- forM [1 .. 6] $ \i -> MVector.read result i
    putStrLn $ concatMap formatResult $ zip [1..] firstResults

    end <- getCurrentTime
    let totalTime = diffUTCTime end start

    putStrLn $ "Total time: " ++ (show totalTime)

    where
        xy = [0.0 :+ 0.0 | _ <- [1 .. (size `div` 2) - 1]] <>
             [1.0 :+ 0.0 | _ <- [size `div` 2 .. size - 1]]

        log2FftSize :: Int
        log2FftSize = 12

        fftRepeat :: Integer
        fftRepeat = 10000

        size :: Int
        size = 1 `shift` log2FftSize

        formatResult :: (Int, Complex Float) -> String
        formatResult r = (show $ fst r) ++ " -> " ++ (show $ snd r) ++ "\n"

{-

fn main() {

// FFT
    println!("{} piece(s) of {} pt FFT;    {} ms/piece\n\n", FFT_REPEAT, SIZE, milliseconds/FFT_REPEAT as f64);

    for i in 0..6 {
	println!("{}  {} {}", i, xy_out_fft[i].re, xy_out_fft[i].im);
    }
}
-}