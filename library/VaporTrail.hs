{-# LANGUAGE RankNTypes #-}
module VaporTrail (main) where

import Control.Arrow
import Control.Monad
import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Foldable
import Data.Machine hiding (fold, zipWith)
import Data.Word
import GHC.Float (float2Double)
import System.Environment
import VaporTrail.Codec.Bits
import VaporTrail.Codec.PCM
import VaporTrail.Codec.Type
import VaporTrail.Codec.UCode
import VaporTrail.Codec.FEC
import VaporTrail.Filter.SignalLock
import Data.Semigroup
import Data.List
import Debug.Trace

sampleRate :: Int
sampleRate = 48000

dataRate :: Int
dataRate = 2000

codecPcmUcode :: Codec [Word8] [Word8]
codecPcmUcode =
  fec 16 32 .
  bitsLE .
  ucode sampleRate dataRate .
  codec id (lockSignal dataRate sampleRate) .
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
  in tones . codecEnc (fec 32 64 . bitsLE . ucode sampleRate dataRate)

sourceByteString :: ByteString -> Source Word8
sourceByteString bs = source (B.unpack bs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["enc_pcm"] -> do
      input <- B.getContents
      let output = B.pack (codecEnc codecPcmUcode (B.unpack input))
      B.putStr output
    ["enc"] -> do
      input <- B.getContents
      let output = encodeTone (B.unpack input)
      B.putStr (toLazyByteString output)
    ["dec"] -> do
      input <- B.getContents
      let output = B.pack (codecDec codecPcmUcode (B.unpack input))
      B.putStr output
    _ -> putStrLn "Usage: fsk <enc_pcm|enc|dec>"
      
