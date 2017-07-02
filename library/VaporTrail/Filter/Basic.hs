{-# LANGUAGE RankNTypes #-}
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

import VaporTrail.Filter.Types
import Data.Semigroup

-- https://en.wikipedia.org/wiki/Low-pass_filter#Discrete-time_realization
lowPass6db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
lowPass6db hz env =
  let dt = 1 / fromIntegral (getSampleRate env)
      rc = 1 / (2 * pi * hz)
      alpha = dt / (dt + rc)
      step prev cur = prev * (1 - alpha) + cur
  in scanl1 step . map (* alpha)
{-# INLINABLE lowPass6db #-}

-- https://en.wikipedia.org/wiki/High-pass_filter#Algorithmic_implementation
highPass6db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
highPass6db hz env xs =
  let dt = 1 / fromIntegral (getSampleRate env)
      rc = 1 / (2 * pi * hz)
      alpha = rc / (rc + dt)
      step prev dx = alpha * (prev + dx)
  in case xs of
       [] -> []
       xs -> scanl step (head xs) (zipWith subtract xs (tail xs))

bandPass6db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
bandPass6db hz env = highPass6db hz env . lowPass6db hz env

-- Order 1 = 6db
-- Order 2 = 12db
-- Order 3 = 24db
-- etc...
order :: Int -> ([Float] -> [Float]) -> ([Float] -> [Float])
order n = appEndo (stimesMonoid (n - 1) (Endo (\f -> f . f)))
{-# INLINABLE order #-}

lowPass :: HasSampleRate env => Int -> Float -> env -> [Float] -> [Float]
lowPass o hz env = order o (lowPass6db hz env)
{-# INLINABLE lowPass #-}

lowPass12db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
lowPass12db = lowPass 2
{-# INLINABLE lowPass12db #-}

lowPass24db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
lowPass24db = lowPass 3

highPass :: HasSampleRate env => Int -> Float -> env -> [Float] -> [Float]
highPass o hz env = order o (highPass6db hz env)

highPass12db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
highPass12db = highPass 2

highPass24db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
highPass24db = highPass 3

bandPass :: HasSampleRate env => Int -> Float -> env -> [Float] -> [Float]
bandPass o hz env = order o (bandPass6db hz env)

bandPass12db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
bandPass12db = bandPass 2

bandPass24db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
bandPass24db = bandPass 3
