{-# LANGUAGE RankNTypes #-}
module VaporTrail.Filter.SignalLock (lockSignal) where

import VaporTrail.Filter.Fourier
import VaporTrail.Filter.Basic
import Data.Machine
import Control.Monad
import Data.Foldable
import Control.Category hiding ((.), id)

sigStrength :: Float -> Int -> [Float] -> Float
sigStrength hz sampleRate xs =
  let bin = hz * (fromIntegral dftSize / fromIntegral sampleRate)
  in goertzelPower bin dftSize xs
{-# INLINABLE sigStrength #-}

lockChunk :: Category k => Float -> Int -> Plan (k Float) o ([Float], Float)
lockChunk hz sampleRate = do
  c <- replicateM dftSize await
  return (c, sigStrength hz sampleRate c)
{-# INLINABLE lockChunk #-}

acquireSignal :: Category k => Float -> Int -> Plan (k Float) o [Float]
acquireSignal hz sampleRate = do
  (chunk, sigLevel) <- lockChunk hz sampleRate
  if sigLevel < 0.9
    then acquireSignal hz sampleRate
    else return chunk
{-# INLINABLE acquireSignal #-}

maintainLock :: Category k => Float -> Int -> Plan (k Float) o [Float]
maintainLock hz sampleRate = do
  (chunk, sigLevel) <- lockChunk hz sampleRate
  if sigLevel < 0.3
    then acquireSignal hz sampleRate
    else return chunk
{-# INLINABLE maintainLock #-}


lockOn :: Float -> Int -> Process Float Float
lockOn hz sampleRate =
  construct $ do
    firstChunk <- acquireSignal hz sampleRate
    forM_ firstChunk yield
    forever $ do
      chunk <- maintainLock hz sampleRate
      forM_ chunk yield
{-# INLINABLE lockOn #-}

calcSignalNormalizer :: Category k => Plan (k Float) Float (Float -> Float)
calcSignalNormalizer = do
  samples <- replicateM (10 * dftSize) await
  let dco = sum samples / (dftSizeF * 10)
      maxAmp = foldl' (\prev x -> max prev (abs (x - dco))) 0 samples
      normalize x = (x - dco) / maxAmp
  forM_ samples (\sample -> yield (normalize sample))
  return normalize
{-# INLINABLE calcSignalNormalizer #-}

skipStartupNoise :: Category k => Plan (k Float) o ()
skipStartupNoise = replicateM_ (10 * dftSize) await

normalizeSignal :: Process Float Float
normalizeSignal =
  construct $ do
    normalize <- calcSignalNormalizer
    forever $ do
      x <- await
      yield (normalize x)
{-# INLINABLE normalizeSignal #-}

dftSize :: Int
dftSize = 192
{-# INLINABLE dftSize #-}

dftSizeF :: Float
dftSizeF = fromIntegral dftSize
{-# INLINABLE dftSizeF #-}

lockSignal :: Int -> Int -> Process Float Float
lockSignal dataRate sampleRate =
  lockOn (fromIntegral dataRate) sampleRate ~>
  lowPass12db (fromIntegral dataRate * 2) sampleRate ~>
  normalizeSignal
{-# INLINABLE lockSignal #-}
