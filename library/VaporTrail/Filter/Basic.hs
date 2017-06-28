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

import Data.Machine
import VaporTrail.Filter.Types
import Data.Function

-- https://en.wikipedia.org/wiki/Low-pass_filter#Discrete-time_realization
lowPass6db :: HasSampleRate env => Float -> env -> Process Float Float
lowPass6db hz env =
  let dt = 1 / fromIntegral (getSampleRate env)
      rc = 1 / (2 * pi * hz)
      alpha = dt / (dt + rc)
      step prev = do
        yield prev
        x <- await
        step (prev + alpha * (x - prev))
  in construct
       (do x0 <- await
           step (x0 * alpha))

-- https://en.wikipedia.org/wiki/High-pass_filter#Algorithmic_implementation
highPass6db :: HasSampleRate env => Float -> env -> Process Float Float
highPass6db hz env =
  let dt = 1 / fromIntegral (getSampleRate env)
      rc = 1 / (2 * pi * hz)
      alpha = rc / (rc + dt)
      step prevY prevX = do
        yield prevY
        curX <- await
        let dx = curX - prevX
        step (alpha * (prevY + dx)) curX
  in construct
       (do x0 <- await
           step x0 x0)

bandPass6db :: HasSampleRate env => Float -> env -> Process Float Float
bandPass6db hz env = highPass6db hz env <~ lowPass6db hz env

-- Order 1 = 6db
-- Order 2 = 12db
-- Order 3 = 24db
-- etc...
order :: Int -> Process Float Float -> Process Float Float
order 0 f = f
order n f = order (n - 1) f <~ order (n - 1) f

lowPass :: HasSampleRate env => Int -> Float -> env -> Process Float Float
lowPass o hz env = order o (lowPass6db hz env)

lowPass12db :: HasSampleRate env => Float -> env -> Process Float Float
lowPass12db = lowPass 2

lowPass24db :: HasSampleRate env => Float -> env -> Process Float Float
lowPass24db = lowPass 3

highPass :: HasSampleRate env => Int -> Float -> env -> Process Float Float
highPass o hz env = order o (highPass6db hz env)

highPass12db :: HasSampleRate env => Float -> env -> Process Float Float
highPass12db = highPass 2

highPass24db :: HasSampleRate env => Float -> env -> Process Float Float
highPass24db = highPass 3

bandPass :: HasSampleRate env => Int -> Float -> env -> Process Float Float
bandPass o hz env = order o (bandPass6db hz env)

bandPass12db :: HasSampleRate env => Float -> env -> Process Float Float
bandPass12db = bandPass 2

bandPass24db :: HasSampleRate env => Float -> env -> Process Float Float
bandPass24db = bandPass 3
