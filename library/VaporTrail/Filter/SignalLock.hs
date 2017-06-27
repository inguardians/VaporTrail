{-# LANGUAGE RankNTypes #-}
module VaporTrail.Filter.SignalLock (lockSignal) where

import VaporTrail.Filter.Fourier
import VaporTrail.Filter.Basic
import Data.Machine
import Control.Monad
import Data.Complex

lockChunk :: Float -> Int -> Plan (Is Float) o ([Float], Float)
lockChunk hz sampleRate = do
  c <- replicateM dftSize await
  let bin = hz / (fromIntegral sampleRate / dftSizeF)
      s = magnitude (goertzel bin dftSize c)
      a = sum (map abs c) / dftSizeF
      sigLevel = (s * 2) / dftSizeF / a
  return (c, sigLevel)

acquireSignal :: Float -> Int -> Plan (Is Float) o ()
acquireSignal hz sampleRate = do
  (_, sigLevel) <- lockChunk hz sampleRate
  when (sigLevel < 0.9) (acquireSignal hz sampleRate)

maintainLock :: Float -> Int -> Plan (Is Float) o [Float]
maintainLock hz sampleRate = do
  (chunk, sigLevel) <- lockChunk hz sampleRate
  guard (sigLevel > 0.3)
  return chunk

lockedSignal :: Float -> Int -> Process Float Float
lockedSignal hz sampleRate =
  construct $ do
    acquireSignal hz sampleRate
    skipStartupNoise
    forever $ do
      chunk <- maintainLock hz sampleRate
      mapM_ yield chunk

calcSignalNormalizer :: Plan (Is Float) Float (Float -> Float)
calcSignalNormalizer = do
  sample <- replicateM (10 * dftSize) await
  let dco = sum sample / (dftSizeF * 10)
      maxAmp = maximum (map (\x -> abs (x - dco)) sample)
      normalize x = (x - dco) / maxAmp
  mapM_ (yield . normalize) sample
  return normalize

skipStartupNoise :: Plan (Is Float) o ()
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
