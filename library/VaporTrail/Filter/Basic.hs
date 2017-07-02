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
lowPass6db _ _ [] = []
lowPass6db hz env xs =
  let dt = 1 / fromIntegral (getSampleRate env)
      rc = 1 / (2 * pi * hz)
      alpha = dt / (dt + rc)
      step prev cur = prev * (1 - alpha) + cur
  in scanl1 step (map (* alpha) xs)
{-# INLINABLE lowPass6db #-}

-- https://en.wikipedia.org/wiki/High-pass_filter#Algorithmic_implementation
highPass6db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
highPass6db _ _ [] = []
highPass6db hz env xs =
  let dt = 1 / fromIntegral (getSampleRate env)
      rc = 1 / (2 * pi * hz)
      alpha = rc / (rc + dt)
      step prev dx = alpha * (prev + dx)
  in scanl1 step (zipWith subtract (0 : xs) xs)
{-# INLINABLE highPass6db #-}

bandPass6db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
bandPass6db hz env = highPass6db hz env . lowPass6db hz env
{-# INLINABLE bandPass6db #-}

-- Order 0 = 6db
-- Order 1 = 12db
-- Order 2 = 24db
-- etc...
exponentiate :: Int -> ([Float] -> [Float]) -> ([Float] -> [Float])
exponentiate n = appEndo (stimesMonoid n (Endo (\f -> f . f)))
{-# INLINABLE exponentiate #-}

lowPass :: HasSampleRate env => Int -> Float -> env -> [Float] -> [Float]
lowPass o hz env = exponentiate o (lowPass6db hz env)
{-# INLINABLE lowPass #-}

lowPass12db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
lowPass12db hz env =
  let lp6 = lowPass6db hz env
  in lp6 . lp6
{-# INLINABLE lowPass12db #-}

lowPass24db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
lowPass24db hz env =
  let lp12 = lowPass12db hz env
  in lp12 . lp12
{-# INLINABLE lowPass24db #-}

highPass :: HasSampleRate env => Int -> Float -> env -> [Float] -> [Float]
highPass o hz env = exponentiate o (highPass6db hz env)
{-# INLINABLE highPass #-}

highPass12db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
highPass12db hz env =
  let hp6 = highPass6db hz env
  in hp6 . hp6
{-# INLINABLE highPass12db #-}

highPass24db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
highPass24db hz env =
  let hp12 = highPass12db hz env
  in hp12 . hp12
{-# INLINABLE highPass24db #-}

bandPass :: HasSampleRate env => Int -> Float -> env -> [Float] -> [Float]
bandPass o hz env = exponentiate o (bandPass6db hz env)
{-# INLINABLE bandPass #-}

bandPass12db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
bandPass12db hz env =
  let bp6 = bandPass6db hz env
  in bp6 . bp6
{-# INLINABLE bandPass12db #-}

bandPass24db :: HasSampleRate env => Float -> env -> [Float] -> [Float]
bandPass24db hz env =
  let bp12 = bandPass12db hz env
  in bp12 . bp12
{-# INLINABLE bandPass24db #-}
