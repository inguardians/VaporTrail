module VaporTrail.Filter.Basic
  ( lowPass
  , lowPass6db
  , lowPass12db
  , lowPass24db
  , highPass
  , highPass6db
  , highPass12db
  , highPass24db
  , bandPass
  , bandPass6db
  , bandPass12db
  , bandPass24db
  ) where

import Data.Semigroup
import Data.List

-- https://en.wikipedia.org/wiki/Low-pass_filter#Discrete-time_realization
lowPass6db :: Floating a => Int -> a -> [a] -> [a]
lowPass6db _ _ [] = []
lowPass6db sampleRate hz (x:xs) = scanl' f (x * a) xs
  where
    dt = 1 / fromIntegral sampleRate
    rc = 1 / (2 * pi * hz)
    a = dt / (dt + rc)
    f acc x = acc + (x - acc) * a

-- https://en.wikipedia.org/wiki/High-pass_filter#Algorithmic_implementation
highPass6db :: Floating a => Int -> a -> [a] -> [a]
highPass6db _ _ [] = []
highPass6db sampleRate hz xs = scanl' f (head xs) (zipWith subtract xs (tail xs))
  where
    dt = 1 / fromIntegral sampleRate
    rc = 1 / (2 * pi * hz)
    a = dt / (dt + rc)
    f acc dx = a * (acc + dx)

bandPass6db :: Floating a => Int -> a -> [a] -> [a]
bandPass6db sampleRate hz = highPass6db sampleRate hz . lowPass6db sampleRate hz

-- Order 1 = 6db
-- Order 2 = 12db
-- Order 3 = 24db
-- etc...
order :: Int -> (a -> a) -> a -> a
order n f = appEndo (stimesMonoid (2 ^ (n - 1)) (Endo f))

lowPass :: Floating a => Int -> Int -> a -> [a] -> [a]
lowPass n sampleRate hz = order n (lowPass6db sampleRate hz)

lowPass12db :: Floating a => Int -> a -> [a] -> [a]
lowPass12db = lowPass 2

lowPass24db :: Floating a => Int -> a -> [a] -> [a]
lowPass24db = lowPass 3

highPass :: Floating a => Int -> Int -> a -> [a] -> [a]
highPass n sampleRate hz = order n (highPass6db sampleRate hz)

highPass12db :: Floating a => Int -> a -> [a] -> [a]
highPass12db = highPass 2

highPass24db :: Floating a => Int -> a -> [a] -> [a]
highPass24db = highPass 3

bandPass :: Floating a => Int -> Int -> a -> [a] -> [a]
bandPass n sampleRate hz = order n (bandPass6db sampleRate hz)

bandPass12db :: Floating a => Int -> a -> [a] -> [a]
bandPass12db = bandPass 2

bandPass24db :: Floating a => Int -> a -> [a] -> [a]
bandPass24db = bandPass 3
