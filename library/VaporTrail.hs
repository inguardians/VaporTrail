{-# LANGUAGE RankNTypes #-}
module VaporTrail (main) where

import Control.Monad
import Data.Bits
import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Complex
import Data.Foldable
import Data.Functor.Identity
import Data.Int
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import qualified Data.Machine as Machine
import Data.Machine hiding (fold, zipWith)
import Data.Machine.Group
import Data.Machine.Runner
import Data.Maybe
import Data.Semigroup
import Data.Word
import Debug.Trace
import GHC.Float (float2Double)
import System.Environment
import qualified VaporTrail.Filter.Basic as Filter.Basic
import VaporTrail.Filter.Fourier


runFilter :: (Float -> Int -> Process Float Float) -> Float -> [Float] -> [Float]
runFilter m hz xs = run (m hz sampleRate <~ source xs)

lowPass6db :: Float -> [Float] -> [Float]
lowPass6db = runFilter Filter.Basic.lowPass6db

lowPass12db ::Float -> [Float] -> [Float]
lowPass12db = runFilter Filter.Basic.lowPass12db

lowPass24db ::Float -> [Float] -> [Float]
lowPass24db = runFilter Filter.Basic.lowPass24db

highPass6db ::Float -> [Float] -> [Float]
highPass6db = runFilter Filter.Basic.highPass6db

highPass12db ::Float -> [Float] -> [Float]
highPass12db = runFilter Filter.Basic.highPass12db

highPass24db ::Float -> [Float] -> [Float]
highPass24db = runFilter Filter.Basic.highPass24db

bandPass6db ::Float -> [Float] -> [Float]
bandPass6db = runFilter Filter.Basic.bandPass6db

bandPass12db ::Float -> [Float] -> [Float]
bandPass12db = runFilter Filter.Basic.bandPass12db

bandPass24db ::Float -> [Float] -> [Float]
bandPass24db = runFilter Filter.Basic.bandPass24db



data TWord = Sync | One | Zero | Empty deriving (Eq,Read,Show)

tAmp :: Fractional a => TWord -> a
tAmp Sync = 5 / 5
tAmp One = 3 / 5
tAmp Zero = 2 / 5
tAmp Empty = 0

sampleRate = 48000

dataRate = 2000

-- TODO factor this out
lockSignal :: Process Float Float
lockSignal =
  let dftSize = 192
      dftSizeF = fromIntegral dftSize
      lockChunk = do
        c <- replicateM dftSize await
        let s = magnitude (goertzel (dftSizeF / 24) dftSize c)
            a = sum (map abs c) / dftSizeF
            sigLevel = (s * 2) / dftSizeF / a
        return (c, sigLevel)
      acquireSignal = do
        (chunk, sigLevel) <- lockChunk
        if sigLevel < 0.9
          then acquireSignal
          else pure ()
      maintainLock = do
        (chunk, sigLevel) <- lockChunk
        guard (sigLevel > 0.3)
        return chunk
      lockedSignal =
        construct $ do
          acquireSignal
          skipStartupNoise
          forever $ do
            chunk <- maintainLock
            mapM_ yield chunk
      calcSignalNormalizer = do
        sample <- replicateM (10 * dftSize) await
        let dco = sum sample / (dftSizeF * 10)
            maxAmp = maximum (map (\x -> abs (x - dco)) sample)
            normalize x = (x - dco) / maxAmp
        mapM_ (yield . normalize) sample
        return normalize
      skipStartupNoise = replicateM_ (10 * dftSize) await
      normalizedSignal = construct $ do
        skipStartupNoise
        normalize <- calcSignalNormalizer
        forever $ do
          x <- await
          yield (normalize x)
  in normalizedSignal <~
     Filter.Basic.bandPass12db (fromIntegral dataRate) sampleRate <~
     lockedSignal

-- Temporary until lockSignal becomes a Process
decodeSignal :: Process Float TWord
decodeSignal = fskDecode <~ lockSignal

tFromAmp :: Float -> TWord
tFromAmp x
  | x >= 0.8 = Sync
  | x >= 0.5 = One
  | x >= 0.1 = Zero
  | otherwise = Empty

tChunkSize = 256

tToBit :: TWord -> Maybe Bool
tToBit Sync = Nothing
tToBit One = Just True
tToBit Zero = Just False
tToBit Empty = Nothing

tToBits :: Process TWord Bool
tToBits =
  construct . forever $ do
    word <- await
    mapM_ yield (tToBit word)

tFromBit :: Bool -> TWord
tFromBit True = One
tFromBit False = Zero


tFromBits :: Process Bool TWord
tFromBits =
  construct $ do
    replicateM_ 48000 (yield Sync) -- Header
    forever $ do
      replicateM_ 4 (yield Sync) -- Chunk header
      replicateM_ tChunkSize $ do
        bit <- await
        yield (tFromBit bit)


-- # == 3/3 T
-- # == 2/3 F
-- # == 1/3 -
fskDecode :: Process Float TWord
fskDecode =
  let notEmpty x = tFromAmp (abs x) /= Empty
      polarity x = x < 0
      eqPolarity x y = polarity x == polarity y
  in mapping tFromAmp <~ groupingOn eqPolarity (largest <~ mapping abs) <~
     filtered notEmpty

fskEncode :: Process TWord Float
fskEncode = construct . forever $ do
  positive <- await
  yield (tAmp positive)
  negative <- await
  yield (-(tAmp negative))

fskEncodePCM :: Int -> Int -> Process TWord Float
fskEncodePCM hz sr =
  let duration = sr `div` hz `div` 2
      extendSamples =
        construct . forever $ do
          x <- await
          replicateM_ duration (yield x)
  in extendSamples <~ fskEncode

fskEncodeTone :: Int -> Process TWord Builder
fskEncodeTone hz =
  let duration = fromIntegral $ 10 ^ 9 `div` hz `div` 2
      excursion = 12000
      tones =
        construct . forever $ do
          tone <- await
          yield (doubleLE (float2Double (tone * excursion)))
          yield (word32LE duration)
          yield (word32LE 0)
  in tones <~ fskEncode

fskEncodePCM48 = fskEncodePCM dataRate sampleRate

fskEncodeTone48 = fskEncodeTone dataRate

removeDCOffset :: [Float] -> [Float]
removeDCOffset xs = map (subtract (mean xs)) xs

mean :: [Float] -> Float
mean xs = sum xs / fromIntegral (length xs)

normalize xs = map (/ sf) xs
  where sf = traceShowId . maximum $ map abs xs

toPCMS16LE :: Process Float Builder
toPCMS16LE =
  let clamp x
        | x < -1 = -1
        | x > 1 = 1
        | otherwise = x
      toS16LE x = int16LE (floor (32767 * clamp x))
  in mapping toS16LE

sourceByteString :: ByteString -> Source Word8
sourceByteString = unfold B.uncons

fromPCMS16LE :: Process Word8 Float
fromPCMS16LE =
  construct . forever $ do
    l <- await
    h <- await
    let lo = fromIntegral l :: Int16
        hi = fromIntegral h :: Int16
    yield (fromIntegral (lo + hi * 256) / 32767)


bitsToBytes :: Process Bool Word8
bitsToBytes = mapping toWord <~ buffered 8
  where
    f n True w = setBit w n
    f _ False w = w
    toWord = foldl' (flip ($)) 0 . zipWith f [0 ..]

bytesToBits :: Process Word8 Bool
bytesToBits =
  construct . forever $ do
    word <- await
    forM_ [0 .. 7] (\n -> yield (testBit word n))

readPCM :: IO [Float]
readPCM = do
  input <- fmap sourceByteString B.getContents
  return (run (fromPCMS16LE <~ input))

writePCM :: [Float] -> IO ()
writePCM xs =
  B.putStr (toLazyByteString (runIdentity (foldT (toPCMS16LE <~ source xs))))

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["enc_pcm"] -> do
      input <- fmap sourceByteString B.getContents
      let output = fold (toPCMS16LE <~ fskEncodePCM48 <~ tFromBits <~ bytesToBits <~ input)
      B.putStr (toLazyByteString output)
    ["enc"] -> do
      input <- fmap sourceByteString B.getContents
      let output = fold (fskEncodeTone48 <~ tFromBits <~ bytesToBits <~ input)
      B.putStr (toLazyByteString output)
    ["dec"] -> do
        input <- fmap sourceByteString B.getContents
        let output = B.pack (run (bitsToBytes <~ tToBits <~ fskDecode <~ fromPCMS16LE <~ input))
        B.putStr output
    ["lowpass6"] -> (lowPass6db 400 <$> readPCM) >>= writePCM
    ["lowpass12"] -> (lowPass12db 400 <$> readPCM) >>= writePCM
    ["lowpass24"] -> (lowPass24db 400 <$> readPCM) >>= writePCM
    ["highpass6"] -> (highPass6db 400 <$> readPCM) >>= writePCM
    ["highpass12"] -> (highPass12db 400 <$> readPCM) >>= writePCM
    ["highpass24"] -> (highPass24db 400 <$> readPCM) >>= writePCM
    ["bandpass6"] -> (bandPass6db 400 <$> readPCM) >>= writePCM
    ["bandpass12"] -> (bandPass12db 400 <$> readPCM) >>= writePCM
    ["bandpass24"] -> (bandPass24db 400 <$> readPCM) >>= writePCM
    {-["lock"] -> (lockSignal <$> readPCM) >>= writePCM-}
    _ -> putStrLn "Usage: fsk <enc_pcm|enc|dec|lowpass6|lowpass12|lowpass24>"
      
