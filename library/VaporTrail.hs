module VaporTrail (main) where

import Control.Lens
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.List
import Data.Semigroup
import Data.Word
import GHC.Float (float2Double)
import System.Environment
import VaporTrail.Codec.Bits
import VaporTrail.Codec.FEC
import VaporTrail.Codec.PCM
import VaporTrail.Codec.UCode
import VaporTrail.Filter.SignalLock

sampleRate :: Int
sampleRate = 48000

dataRate :: Int
dataRate = 2000

encodePcmUcode :: Iso' [Word8] [Word8]
encodePcmUcode =
  fec 16 32 .
  bitsLE .
  ucode sampleRate dataRate .
  iso id (lockSignal dataRate sampleRate) .
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
  in tones . view (fec 32 64 . bitsLE . ucode sampleRate dataRate)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["enc_pcm"] -> do
      input <- Lazy.getContents
      let output = Lazy.pack (view encodePcmUcode (Lazy.unpack input))
      Lazy.putStr output
    ["enc"] -> do
      input <- Lazy.getContents
      let output = encodeTone (Lazy.unpack input)
      Lazy.putStr (toLazyByteString output)
    ["dec"] -> do
      input <- Lazy.getContents
      let output = Lazy.pack (view (from encodePcmUcode) (Lazy.unpack input))
      Lazy.putStr output
    _ -> putStrLn "Usage: fsk <enc_pcm|enc|dec>"
      
