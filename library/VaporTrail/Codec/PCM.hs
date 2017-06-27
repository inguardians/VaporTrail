module VaporTrail.Codec.PCM (pcms16le) where

import Control.Monad
import Data.Bits
import Data.Int
import Data.Machine
import Data.Word
import VaporTrail.Codec.Type

clip :: Float -> Float
clip x = min 1 (max (-1) x)

pcms16le :: Codec Float Word8
pcms16le =
  Codec
  { codecEnc =
      let toInt16 :: Float -> Int16
          toInt16 x =
            if x < 0
              then floor (32768 * clip x)
              else floor (32767 * clip x)
      in construct . forever $ do
           x <- fmap toInt16 await
           let lo = fromIntegral (x .&. 0xFF)
               hi = fromIntegral (shiftR x 8 .&. 0xFF)
           yield lo
           yield hi
  , codecDec =
      let fromInt16 :: Int16 -> Float
          fromInt16 x =
            if x < 0
              then fromIntegral x / 32768
              else fromIntegral x / 32767
      in construct . forever $ do
           l <- await
           h <- await
           let lo = fromIntegral l :: Int16
               hi = fromIntegral h :: Int16
               x = lo .|. shiftL hi 8
           yield (fromInt16 x)
  }
