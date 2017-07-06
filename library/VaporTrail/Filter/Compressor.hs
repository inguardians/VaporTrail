module VaporTrail.Filter.Compressor where

import Data.List


{-compressor :: Float -> Float -> Float -> Float -> Int -> [Float] -> [Float]-}
{-compressor threshold ratio attack release sampleRate = _-}

limiter :: Float -> Float -> Float -> Int -> [Float] -> [Float]
limiter threshold attack release sampleRate input =
  let attackRate = 1 / max 1 (attack * fromIntegral sampleRate)
      releaseRate = 1 / max 1 (release * fromIntegral sampleRate)
      psi (_, []) = Nothing
      psi (gain, x:xs) =
        let newGain
              | x == 0 = gain
              | abs x * gain > threshold =
                gain * (1 - attackRate) + (threshold / abs x) * attackRate
              | abs x * gain < threshold && gain < 1 =
                gain * (1 - releaseRate) + (threshold / abs x) * releaseRate
              | otherwise = gain
        in Just (newGain * x, (newGain, xs))
  in unfoldr psi (1, input)


colimiter :: Float -> Float -> Float -> Int -> [Float] -> [Float]
colimiter threshold attack release sampleRate input =
  let attackRate = 1 / max 1 (attack * fromIntegral sampleRate)
      releaseRate = 1 / max 1 (release * fromIntegral sampleRate)
      psi (_, []) = Nothing
      psi (gain, x:xs) =
        let newGain
              | x == 0 = gain
              | abs x * gain < threshold =
                gain * (1 - attackRate) + (threshold / abs x) * attackRate
              | abs x * gain > threshold && gain > 1 =
                gain * (1 - releaseRate) + (threshold / abs x) * releaseRate
              | otherwise = gain
        in Just (newGain * x, (newGain, xs))
  in unfoldr psi (1, input)
