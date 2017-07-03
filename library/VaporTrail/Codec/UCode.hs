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


-- Preamble will be a series of ones followed by a single zero
preamble :: [Bool]
preamble = replicate 4000 True ++ [False]

-- We need to see at least half the preamble to know we're good
detectPreamble :: [Bool] -> [Bool]
detectPreamble input =
  let psi (_, []) = Nothing
      psi (i, (x:xs)) =
        if x
          then Just ([], (i + 1, xs))
          else if i >= 2000
                 then Just (xs, (0, []))
                 else Just ([], (0, xs))
  in concat (unfoldr psi (0 :: Int, input))


decode :: [Float] -> [Bool]
decode =
  let notEmpty x = abs x >= zeroThreshold
      polarity x = x < 0
      eqPolarity x y = polarity x == polarity y
      maxAmp = foldr (\x y -> max (abs x) y) 0
  in detectPreamble .
     map (\xs -> maxAmp xs >= oneThreshold) .
     groupBy eqPolarity . filter notEmpty
{-# INLINABLE decode #-}

encode :: Int -> Int -> [Bool] -> [Float]
encode sampleRate dataRate input =
  let samplesPerBit = sampleRate `div` dataRate `div` 2
      phi x (polarity, ys) =
        (-polarity, replicate samplesPerBit (polarity * bitAmp x) <> ys)
  in snd (foldr phi (1, []) (preamble ++ input))
{-# INLINABLE encode #-}

ucode :: Int -> Int -> Iso' [Bool] [Float]
ucode sampleRate dataRate = iso (encode sampleRate dataRate) decode
{-# INLINABLE ucode #-}
