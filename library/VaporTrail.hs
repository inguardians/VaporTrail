module VaporTrail (main) where

import Control.Lens
import Data.Bits
import qualified Data.ByteString as Strict
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.List
import Data.Semigroup
import Data.Word
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Storable as Foreign
import GHC.Float (float2Double)
import System.Environment
import System.IO
import System.Random
import VaporTrail.Codec.FEC
import VaporTrail.Codec.PCM
import VaporTrail.Codec.Packet
import VaporTrail.Codec.UCode
import VaporTrail.Filter.Basic
import VaporTrail.Filter.Compressor
import VaporTrail.Filter.SignalLock

sampleRate :: Int
sampleRate = 48000

dataRate :: Int
dataRate = 2000


-- | Xor each data chunk with noise, using the chunk ID as the seed. The
-- purpose of this is to get a more even distribution of ones and zeroes in
-- the output stream, so the signal locker doesn't try to over-amplify the
-- zeroes
xorNoise :: Iso' [(Int, Strict.ByteString)] [(Int, Strict.ByteString)]
xorNoise =
  let withChunkNoise (n, bytes) =
        let gen = mkStdGen n
            noise = take (Strict.length bytes) (randoms gen)
            output = Strict.pack (zipWith xor (Strict.unpack bytes) noise)
        in (n, output)
  in iso (map withChunkNoise) (map withChunkNoise)

encodePackets :: Iso' [Word8] [Float]
encodePackets = fec 16 32 . xorNoise . packetStream . ucode sampleRate dataRate

encodePcmPackets :: Iso' [Word8] [Word8]
encodePcmPackets =
  encodePackets .
  iso
    id
    ((colimiter 1.0 (4000 / fromIntegral dataRate) 0 sampleRate) .
     lowPass12db (fromIntegral dataRate * 4) sampleRate) .
  pcms16le

encodeTone :: [Word8] -> Builder
encodeTone =
  let duration = fromIntegral (10 ^ (9 :: Int) `div` sampleRate)
      excursion = 12000
      toTone [] = mempty
      toTone xs =
        doubleLE (float2Double (head xs * excursion)) <>
        word32LE (fromIntegral (length xs) * duration) <>
        word32LE 0
      tones = foldMap toTone . group
  in tones . view encodePackets

putUnbuffered :: (Foreign.Storable a, Foldable t) => Handle -> t a -> IO ()
putUnbuffered handle xs =
  let withUnbuffered m = do
        initialBufferMode <- hGetBuffering handle
        hSetBuffering handle NoBuffering
        result <- m
        hSetBuffering handle initialBufferMode
        return result
      putStorable ptr x = do
        Foreign.poke ptr x
        hPutBuf handle ptr (Foreign.sizeOf x)
  in withUnbuffered (Foreign.alloca (\ptr -> mapM_ (putStorable ptr) xs))


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["enc_pcm"] -> do
      input <- Lazy.getContents
      let output = Lazy.pack (view encodePcmPackets (Lazy.unpack input))
      Lazy.putStr output
    ["enc"] -> do
      input <- Lazy.getContents
      let output = encodeTone (Lazy.unpack input)
      Lazy.putStr (toLazyByteString output)
    ["dec"] -> do
      input <- Lazy.getContents
      let output = view (from encodePcmPackets) (Lazy.unpack input)
      putUnbuffered stdout output
    ["tr"] -> do
      input <- Lazy.getContents
      let output =
            Lazy.pack
              (view
                 (from pcms16le .
                  iso
                    ((colimiter
                        1.0
                        (4000 / fromIntegral dataRate)
                        0
                        {-(4000 / fromIntegral dataRate)-}
                        sampleRate) .
                     lowPass12db (fromIntegral dataRate * 4) sampleRate)
                    id .
                  pcms16le)
                 (Lazy.unpack input))
      Lazy.putStr output
    _ -> putStrLn "Usage: fsk <enc_pcm|enc|dec>"
      
