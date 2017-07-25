module VaporTrail.Filter.Fourier (goertzel, goertzelPower) where

import Data.Complex
import Data.Foldable

-- https://en.wikipedia.org/wiki/Goertzel_algorithm
-- TODO document and make clearer
{-goertzel :: Float -> Int -> [Float] -> Complex Float-}
{-goertzel bin dftSize samples = y !! dftSize-}
  {-where-}
    {-w0 = 2 * pi * (bin / fromIntegral dftSize)-}
    {-e = exp 1-}
    {-xs = take dftSize samples ++ repeat 0-}
    {-s = zipWith3 (\x s1 s2 -> x + 2 * cos w0 * s1 - s2) xs (0 : s) (0 : 0 : s)-}
    {-y = zipWith (\s0 s1 -> (s0 :+ 0) - e ** (0 :+ (-w0)) * (s1 :+ 0)) s (tail s)-}


goertzel :: Foldable f => Float -> Int -> f Float -> (Float, Complex Float)
goertzel bin dftSize samples =
  let w0 = 2 * pi * (bin / fromIntegral dftSize)
      e = exp 1
      step (n, total, s2, s1) x =
        if n <= dftSize
          then (n + 1, total + abs x, s1, x + 2 * cos w0 * s1 - s2)
          else (n, total, s2, s1)
      finalize (n, total, s2, s1) =
        if n <= dftSize
          then finalize (step (n, total, s2, s1) 0)
          else (n, total, s2,  s1)
      (_, sampleSum, s2final, s1final) = finalize (foldl' step (0, 0, 0, 0) samples)
      y = (s1final :+ 0) - e ** (0 :+ (-w0)) * (s2final :+ 0)
  in (sampleSum, y)
{-# INLINABLE goertzel #-}

goertzelPower :: Foldable f => Float -> Int -> f Float -> Float
goertzelPower bin dftSize samples =
  let (total, y) = goertzel bin dftSize samples
  in (magnitude y * 2) / total
{-# INLINABLE goertzelPower #-}
