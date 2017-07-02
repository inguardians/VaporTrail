module VaporTrail.Codec.PCM (pcms16le) where

import Control.Lens (Iso', iso)
import Data.Bits
import Data.Int
import Data.List
import Data.Word

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

pcms16leEncode :: [Float] -> [Word8]
pcms16leEncode =
  let phi input ys =
        let sample = toInt16 input
            lo = fromIntegral sample
            hi = fromIntegral (shiftR sample 8)
        in (lo : hi : ys)
  in foldr phi []
{-# INLINABLE pcms16leEncode #-}

pcms16leDecode :: [Word8] -> [Float]
pcms16leDecode =
  let psi (l:h:xs) =
        let lo = fromIntegral l
            hi = shiftL (fromIntegral h) 8
            sample = lo .|. hi
        in Just (fromInt16 sample, xs)
      psi _ = Nothing
  in unfoldr psi
{-# INLINABLE pcms16leDecode #-}


pcms16le :: Iso' [Float] [Word8]
pcms16le = iso pcms16leEncode pcms16leDecode
{-# INLINABLE pcms16le #-}
