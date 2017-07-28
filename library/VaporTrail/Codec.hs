{-# LANGUAGE ScopedTypeVariables #-}
module VaporTrail.Codec
  ( packets
  , packetsPcm
  , encodePacketsPcm
  , decodePacketsPcm
  , encodePacketsTone
  ) where

import Control.Lens
import Data.Bits
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.List
import Data.Semigroup
import Data.Word
import GHC.Float (float2Double)
import System.Random
import VaporTrail.Codec.FEC
import VaporTrail.Codec.PCM
import VaporTrail.Codec.Packet
import VaporTrail.Codec.UCode
import VaporTrail.Filter.Basic
import VaporTrail.Filter.Compressor
  
sampleRate :: Int
sampleRate = 48000

dataRate :: Int
dataRate = 2000

fecDataChunks :: Int
fecDataChunks = 16

fecTotalChunks :: Int
fecTotalChunks = 32

-- | Xor each data chunk with noise, using the chunk ID as the seed. The
-- purpose of this is to get a more even distribution of ones and zeroes in
-- the output stream, so the signal locker doesn't try to over-amplify the
-- zeroes
xorNoise :: Iso' [FecChunk] [FecChunk]
xorNoise =
  let withChunkNoise (FecChunk (n, bytes)) =
        let gen = mkStdGen n
            noise = take (Strict.length bytes) (randoms gen)
            output = Strict.pack (zipWith xor (Strict.unpack bytes) noise)
        in (FecChunk (n, output))
  in iso (map withChunkNoise) (map withChunkNoise)

packets :: Iso' [[Word8]] [Float]
packets =
  fec (fromIntegral fecDataChunks) (fromIntegral fecTotalChunks) .
  xorNoise . packetStream . ucode sampleRate dataRate

packetsPcm :: Iso' [[Word8]] [Word8]
packetsPcm =
  packets .
  iso
    id
    ((colimiter 1.0 (4000 / fromIntegral dataRate) 0 sampleRate) .
     lowPass12db (fromIntegral dataRate * 4) sampleRate) .
  pcms16le

encodePacketsPcm :: [[Word8]] -> [Word8]
encodePacketsPcm = view packetsPcm

decodePacketsPcm :: [Word8] -> [[Word8]]
decodePacketsPcm = view (from packetsPcm)

encodePacketsTone :: [[Word8]] -> [Word8]
encodePacketsTone =
  let duration = fromIntegral (10 ^ (9 :: Int) `div` sampleRate)
      excursion = 12000
      toTone [] = mempty
      toTone xs =
        Builder.doubleLE (float2Double (head xs * excursion)) <>
        Builder.word32LE (fromIntegral (length xs) * duration) <>
        Builder.word32LE 0
      tones = foldMap toTone . group
  in Lazy.unpack . Builder.toLazyByteString . tones . view packets
