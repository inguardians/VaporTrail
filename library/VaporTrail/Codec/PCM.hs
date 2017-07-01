module VaporTrail.Codec.PCM (pcms16le) where

import Data.Bits
import Data.Int
import Data.Machine
import Data.Word
import VaporTrail.Codec.Type

clip :: Float -> Float
clip sample = min 1 (max (-1) sample)
{-# INLINABLE clip #-}

toInt16 :: Float -> Int16
toInt16 sample =
  if sample < 0
    then floor (32768 * clip sample)
    else floor (32767 * clip sample)
{-# INLINABLE toInt16 #-}


fromInt16 :: Int16 -> Float
fromInt16 sample =
  if sample < 0
    then fromIntegral sample / 32768
    else fromIntegral sample / 32767
{-# INLINABLE fromInt16 #-}

pcms16leEncode :: Process Float Word8
pcms16leEncode =
  repeatedly $ do
    input <- await
    let sample = toInt16 input
        lo = fromIntegral sample
        hi = fromIntegral (shiftR sample 8)
    yield lo
    yield hi
{-# INLINABLE pcms16leEncode #-}

pcms16leDecode :: Process Word8 Float
pcms16leDecode =
  repeatedly $ do
    l <- await
    h <- await
    let lo = fromIntegral l
        hi = shiftL (fromIntegral h) 8
        sample = lo .|. hi
    yield (fromInt16 sample)
{-# INLINABLE pcms16leDecode #-}


pcms16le :: Codec Float Word8
pcms16le = codec pcms16leEncode pcms16leDecode
{-# INLINABLE pcms16le #-}
