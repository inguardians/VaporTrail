{-# LANGUAGE RankNTypes #-}
module VaporTrail.Codec.UCode (ucode) where

import VaporTrail.Codec.Type
import Data.Machine
import Data.Machine.Group
import Control.Monad

data TWord = One | Zero | Empty deriving (Eq, Read, Show)

tRange :: TWord -> (Float, Float)
tRange One = (2/3, 1.0)
tRange Zero = (1/3, 2/3)
tRange Empty = (0, 1/3)
{-# INLINABLE tRange #-}

tAmp :: TWord -> Float
tAmp w =
  let (lo, hi) = tRange w
  in (lo + hi) / 2
{-# INLINABLE tAmp #-}

tFromAmp :: Float -> TWord
tFromAmp x
  | x >= fst (tRange One) = One
  | x >= fst (tRange Zero) = Zero
  | otherwise = Empty
{-# INLINABLE tFromAmp #-}

tToBit :: TWord -> Maybe Bool
tToBit Empty = Nothing
tToBit One = Just True
tToBit Zero = Just False
{-# INLINABLE tToBit #-}

tToBits :: Process TWord Bool
tToBits =
  repeatedly $ do
    word <- await
    case tToBit word of
      Just x -> yield x
      _ -> return ()
{-# INLINABLE tToBits #-}

tFromBit :: Bool -> TWord
tFromBit True = One
tFromBit False = Zero
{-# INLINABLE tFromBit #-}

tFromBits :: Process Bool TWord
tFromBits = mapping tFromBit
{-# INLINABLE tFromBits #-}

decode :: Process Float TWord
decode =
  let notEmpty x = tFromAmp (abs x) /= Empty
      polarity x = x < 0
      eqPolarity x y = polarity x == polarity y
  in filtered notEmpty ~> groupingOn eqPolarity (mapping abs ~> largest) ~>
     mapping tFromAmp
{-# INLINABLE decode #-}

encode :: Int -> Int -> Process TWord Float
encode sampleRate dataRate =
  let samplesPerBit = sampleRate `div` dataRate `div` 2
  in repeatedly $ do
       positive <- await
       replicateM_ samplesPerBit (yield (tAmp positive))
       negative <- await
       replicateM_ samplesPerBit (yield (-(tAmp negative)))
{-# INLINABLE encode #-}

ucode :: Int -> Int -> Codec Bool Float
ucode sampleRate dataRate =
  codec (tFromBits ~> encode sampleRate dataRate) (decode ~> tToBits)
{-# INLINABLE ucode #-}
