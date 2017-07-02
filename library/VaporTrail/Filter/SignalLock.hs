module VaporTrail.Filter.SignalLock (lockSignal) where

import VaporTrail.Filter.Fourier
import VaporTrail.Filter.Basic
import Data.Reflection (foldMapBy)
import Data.Semigroup
import Data.List

sigStrength :: Float -> Int -> [Float] -> Float
sigStrength hz sampleRate xs =
  let bin = hz * (fromIntegral dftSize / fromIntegral sampleRate)
  in goertzelPower bin dftSize xs
{-# INLINABLE sigStrength #-}

lockOn :: Float -> Int -> [Float] -> [Float]
lockOn hz sampleRate samples =
  let str = sigStrength hz sampleRate
      acquireSignal x = str x >= 0.9
      maintainLock x = str x >= 0.3
      psi (xs, sigTest) =
        case splitAt dftSize xs of
          (h, t)
            | length h == dftSize ->
              Just
                (if sigTest h
                   then (h, (t, maintainLock))
                   else ([], (t, acquireSignal)))
          _ -> Nothing
  in concat (unfoldr psi (samples, acquireSignal))
{-# INLINABLE lockOn #-}

normalizeSignal :: [Float] -> [Float]
normalizeSignal xs =
  let samples = take (10 * dftSize) xs
      dco =
        let (Sum s, Sum l) = foldMap (\x -> (Sum x, Sum 1)) samples
        in s / l
      Max maxAmp = foldMapBy (<>) (Max 0) (\x -> Max (abs (x - dco))) samples
      normalize x = (x - dco) / maxAmp
  in map normalize xs
{-# INLINABLE normalizeSignal #-}

dftSize :: Int
dftSize = 192
{-# INLINABLE dftSize #-}

lockSignal :: Int -> Int -> [Float] -> [Float]
lockSignal dataRate sampleRate =
  normalizeSignal .
  lowPass12db (fromIntegral dataRate * 2) sampleRate .
  lockOn (fromIntegral dataRate) sampleRate
{-# INLINABLE lockSignal #-}
