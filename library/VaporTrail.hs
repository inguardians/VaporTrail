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

sampleRate :: Int
sampleRate = 48000

dataRate :: Int
dataRate = 2000

codecPcmUcode :: Codec Word8 Word8
codecPcmUcode =
  fec 16 32 >>>
  bitsLE >>>
  ucode sampleRate dataRate >>>
  codecLmap echo (lockSignal dataRate sampleRate) pcms16le

encodeTone :: Process Word8 Builder
encodeTone =
  let duration = fromIntegral (10 ^ (9 :: Int) `div` sampleRate)
      excursion = 12000
      tones toneSamples prevTone = do
        tone <- await
        if (tone /= prevTone)
          then do
            yield (doubleLE (float2Double (prevTone * excursion)))
            yield (word32LE (toneSamples * duration))
            yield (word32LE 0)
            tones 1 tone
          else tones (toneSamples + 1) tone
  in codecEnc (fec 32 64 >>> bitsLE >>> ucode sampleRate dataRate) ~>
     (construct $ do
        firstTone <- await
        tones 1 firstTone)


sourceByteString :: ByteString -> Source Word8
sourceByteString bs = source (B.unpack bs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["enc_pcm"] -> do
      input <- B.getContents
      let output = fold (sourceByteString input ~> codecEnc codecPcmUcode ~> mapping word8)
      B.putStr (toLazyByteString output)
    ["enc"] -> do
      input <- B.getContents
      let output = fold (sourceByteString input ~> encodeTone)
      B.putStr (toLazyByteString output)
    ["dec"] -> do
      input <- B.getContents
      let decoder = codecDec codecPcmUcode
      let output = B.pack (run (sourceByteString input ~> decoder))
      B.putStr output
    _ -> putStrLn "Usage: fsk <enc_pcm|enc|dec>"
      
