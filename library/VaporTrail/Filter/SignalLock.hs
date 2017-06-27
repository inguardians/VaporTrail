{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
module VaporTrail.Filter.SignalLock (lockSignal) where

import VaporTrail.Filter.Fourier
import VaporTrail.Filter.Basic
import Data.Machine
import Control.Monad
import Data.Complex
import Data.Foldable
import Control.Category hiding ((.), id)


sigStrength :: Float -> Int -> [Float] -> Float
sigStrength hz sampleRate xs =
  let bin = hz / (fromIntegral sampleRate / fromIntegral dftSize)
      s = magnitude (goertzel bin dftSize xs)
      a = foldl' (\acc x -> acc + abs x) 0 xs / fromIntegral dftSize
  in (s * 2) / fromIntegral dftSize / a

lockChunk :: Category k => Float -> Int -> Plan (k Float) o ([Float], Float)
lockChunk hz sampleRate = do
  c <- replicateM dftSize await
  return (c, sigStrength hz sampleRate c)

acquireSignal :: Category k => Float -> Int -> Plan (k Float) o ()
acquireSignal hz sampleRate = do
  (_, sigLevel) <- lockChunk hz sampleRate
  when (sigLevel < 0.9) (acquireSignal hz sampleRate)

maintainLock :: Category k => Float -> Int -> Plan (k Float) o [Float]
maintainLock hz sampleRate = do
  (chunk, sigLevel) <- lockChunk hz sampleRate
  guard (sigLevel > 0.3)
  return chunk

lockedSignal :: Float -> Int -> Process Float Float
lockedSignal hz sampleRate =
  construct $ do
    acquireSignal hz sampleRate
    forever $ do
      chunk <- maintainLock hz sampleRate
      mapM_ yield chunk

calcSignalNormalizer :: Category k => Plan (k Float) Float (Float -> Float)
calcSignalNormalizer = do
  sample <- replicateM (10 * dftSize) await
  let dco = sum sample / (dftSizeF * 10)
      maxAmp = foldl' (\prev x -> max prev (abs (x - dco))) 0 sample
      normalize x = (x - dco) / maxAmp
  mapM_ (yield . normalize) sample
  return normalize

skipStartupNoise :: Category k => Plan (k Float) o ()
skipStartupNoise = replicateM_ (10 * dftSize) await

normalizedSignal :: Process Float Float
normalizedSignal = construct $ do
  skipStartupNoise
  normalize <- calcSignalNormalizer
  forever $ do
    x <- await
    yield (normalize x)

dftSize :: Int
dftSize = 192

dftSizeF :: Float
dftSizeF = fromIntegral dftSize

lockSignal :: Int -> Int -> Process Float Float
lockSignal dataRate sampleRate =
  normalizedSignal <~ lockedSignal (fromIntegral dataRate) sampleRate
