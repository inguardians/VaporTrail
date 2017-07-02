{-# LANGUAGE RankNTypes #-}
module VaporTrail.Codec.UCode (ucode) where

import Data.List
import Data.Semigroup
import Control.Lens (iso, Iso')

zeroThreshold :: Float
zeroThreshold = 1/3
{-# INLINABLE zeroThreshold #-}

oneThreshold :: Float
oneThreshold = 2/3
{-# INLINABLE oneThreshold #-}

bitAmp :: Bool -> Float
bitAmp False = 0.5
bitAmp True = 1.0
{-# INLINABLE bitAmp #-}

decode :: [Float] -> [Bool]
decode =
  let notEmpty x = abs x >= zeroThreshold
      polarity x = x < 0
      eqPolarity x y = polarity x == polarity y
      maxAmp = foldr (\x y -> max (abs x) y) 0
  in map (\xs -> maxAmp xs >= oneThreshold) .
     groupBy eqPolarity . filter notEmpty
{-# INLINABLE decode #-}

encode :: Int -> Int -> [Bool] -> [Float]
encode sampleRate dataRate =
  let samplesPerBit = sampleRate `div` dataRate `div` 2
      phi x (polarity, ys) =
        (-polarity, replicate samplesPerBit (polarity * bitAmp x) <> ys)
  in snd . foldr phi (1, [])
{-# INLINABLE encode #-}

ucode :: Int -> Int -> Iso' [Bool] [Float]
ucode sampleRate dataRate = iso (encode sampleRate dataRate) decode
{-# INLINABLE ucode #-}
