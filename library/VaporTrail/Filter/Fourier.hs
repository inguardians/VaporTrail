module VaporTrail.Filter.Fourier (goertzel) where

import Data.Complex

-- https://en.wikipedia.org/wiki/Goertzel_algorithm
-- TODO document and make clearer
goertzel :: RealFloat a => a -> Int -> [a] -> Complex a
goertzel bin dftSize samples = y !! dftSize
  where
    w0 = 2 * pi * (bin / fromIntegral dftSize)
    e = exp 1
    xs = take dftSize samples ++ repeat 0
    s = zipWith3 (\x s1 s2 -> x + 2 * cos w0 * s1 - s2) xs (0 : s) (0 : 0 : s)
    y = zipWith (\s0 s1 -> (s0 :+ 0) - e ** (0 :+ (-w0)) * (s1 :+ 0)) s (tail s)
