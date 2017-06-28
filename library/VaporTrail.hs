{-# LANGUAGE RankNTypes #-}
module VaporTrail (main) where

import Control.Monad
import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Foldable
import Data.Machine hiding (fold, zipWith)
import Data.Word
import GHC.Float (float2Double)
import System.Environment
import VaporTrail.Filter.SignalLock
import VaporTrail.Codec.Type
import VaporTrail.Codec.UCode
import VaporTrail.Codec.PCM
import VaporTrail.Codec.Bits

sampleRate :: Int
sampleRate = 48000

dataRate :: Int
dataRate = 2000

decodeSignal :: Process Float Bool
decodeSignal = lockSignal dataRate sampleRate ~> codecDec ucode

encodePCM :: Int -> Int -> Process Bool Word8
encodePCM hz sr =
  let duration = sr `div` hz `div` 2
      extendSamples =
        repeatedly $ do
          x <- await
          replicateM_ duration (yield x)
  in codecEnc ucode ~> extendSamples ~> codecEnc pcms16le

encodeTone :: Int -> Process Bool Builder
encodeTone hz =
  let duration = fromIntegral (10 ^ (9 :: Int) `div` hz `div` 2)
      excursion = 12000
      tones =
        repeatedly $ do
          tone <- await
          yield (doubleLE (float2Double (tone * excursion)))
          yield (word32LE duration)
          yield (word32LE 0)
  in codecEnc ucode ~> tones

encodePCM48 :: Process Bool Word8
encodePCM48 = encodePCM dataRate sampleRate

encodeTone48 :: Process Bool Builder
encodeTone48 = encodeTone dataRate

sourceByteString :: ByteString -> Source Word8
sourceByteString = unfold B.uncons

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["enc_pcm"] -> do
      input <- fmap sourceByteString B.getContents
      let output = B.pack (run (input ~> codecEnc bitsLE ~> encodePCM48))
      B.putStr output
    ["enc"] -> do
      input <- fmap sourceByteString B.getContents
      let output = fold (input ~> codecEnc bitsLE ~> encodeTone48)
      B.putStr (toLazyByteString output)
    ["dec"] -> do
      input <- fmap sourceByteString B.getContents
      let output =
            B.pack
              (run
                 (input ~> codecDec pcms16le ~> decodeSignal ~> codecDec bitsLE))
      B.putStr output
    _ -> putStrLn "Usage: fsk <enc_pcm|enc|dec>"
      
