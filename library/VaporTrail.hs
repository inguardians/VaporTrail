module VaporTrail (main) where

import Data.Bits
import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Complex
import Data.Foldable
import Data.Int
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import Data.Maybe
import Data.Semigroup
import Data.Word
import Debug.Trace
import System.Environment
import qualified VaporTrail.Filter.Basic as Filter.Basic
import VaporTrail.Filter.Fourier

lowPass6db :: (Floating a) => a -> [a] -> [a]
lowPass6db = Filter.Basic.lowPass6db sampleRate

lowPass12db :: (Floating a) => a -> [a] -> [a]
lowPass12db = Filter.Basic.lowPass12db sampleRate

lowPass24db :: (Floating a) => a -> [a] -> [a]
lowPass24db = Filter.Basic.lowPass24db sampleRate

highPass6db :: (Floating a) => a -> [a] -> [a]
highPass6db = Filter.Basic.highPass6db sampleRate

highPass12db :: (Floating a) => a -> [a] -> [a]
highPass12db = Filter.Basic.highPass12db sampleRate

highPass24db :: (Floating a) => a -> [a] -> [a]
highPass24db = Filter.Basic.highPass24db sampleRate

bandPass6db :: (Floating a) => a -> [a] -> [a]
bandPass6db = Filter.Basic.bandPass6db sampleRate

bandPass12db :: (Floating a) => a -> [a] -> [a]
bandPass12db = Filter.Basic.bandPass12db sampleRate

bandPass24db :: (Floating a) => a -> [a] -> [a]
bandPass24db = Filter.Basic.bandPass24db sampleRate


data TWord = Sync | One | Zero | Empty deriving (Eq,Read,Show)

tAmp :: Fractional a => TWord -> a
tAmp Sync = 5 / 5
tAmp One = 3 / 5
tAmp Zero = 2 / 5
tAmp Empty = 1 / 5

sampleRate = 48000

dataRate = 2000

lockSignal :: RealFloat a => [a] -> [a]
lockSignal xs = sigNorm
  where
    dftSize = 192
    dftSizeF = fromIntegral dftSize
    f = goertzel (dftSizeF / 24) dftSize
    c = chunksOf dftSize xs
    s = map (magnitude . f) c
    a = map (\x -> sum (map abs x) / dftSizeF) c
    sigLevel = zipWith (\s a -> (s * 2) / dftSizeF / a) s a
    sig =
      drop (dftSize * 10) .
      lowPass12db (fromIntegral dataRate) .
      foldMap fst .
      takeWhile ((> 0.3) . snd) . dropWhile ((< 0.9) . snd) $ zip c sigLevel
    sigNorm = map ((/ n) . subtract dco) sig
      where
        xs = take (dftSize * 10) sig
        dco = sum xs / (dftSizeF * 10)
        n = maximum $ map (abs . subtract dco) xs

fun = normalize . foldMap (\x -> replicate dftSize (magnitude $ f x)) . chunksOf dftSize
  where
    dftSize = 192
    dftSizeF = fromIntegral dftSize
    f = goertzel (dftSizeF / 24) dftSize

decodeSignal :: RealFloat a => [a] -> [TWord]
decodeSignal = fskDecode . lockSignal

tFromAmp :: (Fractional a, Ord a) => a -> TWord
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

tToBits :: [TWord] -> [Bool]
tToBits =
  mapMaybe tToBit . concat . groupBy g
  where
    g x y = (x == Sync) == (y == Sync)

tFromBit :: Bool -> TWord
tFromBit True = One
tFromBit False = Zero

tFromBits :: [Bool] -> [TWord]
tFromBits =
  addSyncHead . addSyncTail . foldMap addSync . chunksOf tChunkSize . map tFromBit
  where
    addSync xs = Sync : Sync : Sync : Sync : xs
    addSyncHead = (replicate 48000 Sync ++)
    addSyncTail = (++ [Sync, Sync, Sync, Sync])

-- # == 3/3 T
-- # == 2/3 F
-- # == 1/3 -
fskDecode :: (Ord a, Fractional a) => [a] -> [TWord]
fskDecode = map (tFromAmp . maximum . map abs) . groupBy eqPolarity . filter notEmpty
  where
    notEmpty x = abs x >= tAmp Empty
    polarity = (< 0)
    eqPolarity x y = polarity x == polarity y

fskEncode :: (Fractional a, Monoid m) => (a -> m) -> [TWord] -> m
fskEncode f = foldMap f . zipWith ($) (cycle $ map (. tAmp) [id, negate])
 
fskEncodePCM :: Fractional a => Int -> Int -> [TWord] -> [a]
fskEncodePCM hz sr = fskEncode (replicate duration)
  where
    duration = sr `div` hz `div` 2

fskEncodeTone :: Int -> [TWord] -> Builder
fskEncodeTone hz = fskEncode (tone . (* excursion))
  where
    duration = fromIntegral $ 10 ^ 9 `div` hz `div` 2
    excursion = 12000
    tone hz = doubleLE hz <> word32LE duration <> word32LE 0

fskEncodePCM48 = fskEncodePCM dataRate sampleRate

fskEncodeTone48 = fskEncodeTone dataRate

removeDCOffset :: (Fractional a, Ord a) => [a] -> [a]
removeDCOffset xs = map (subtract (mean xs)) xs

mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

normalize xs = map (/ sf) xs
  where sf = traceShowId . maximum $ map abs xs

toPCM :: RealFrac a => [a] -> ByteString
toPCM = toLazyByteString . foldMap (int16LE . floor . (* 32767) . clamp)
  where
    clamp x
      | x < -1 = -1
      | x > 1 = 1
      | otherwise = x

fromPCM :: Fractional a => ByteString -> [a]
fromPCM = foldMap s . chunksOf 2 . B.unpack
  where
    s [l, h] = [fromIntegral (lo + hi * 256) / 32767]
      where
        lo = fromIntegral l :: Int16
        hi = fromIntegral h :: Int16
    s _ = []


bitsToBytes :: [Bool] -> [Word8]
bitsToBytes = map toWord . chunksOf 8
  where
    f n True w = setBit w n
    f _ False w = w
    toWord = foldl' (flip ($)) 0 . zipWith f [0 ..]

bytesToBits :: [Word8] -> [Bool]
bytesToBits = foldMap toBits
  where
    toBits x = map (testBit x) [0 .. 7]


readPCM :: IO [Double]
readPCM = fromPCM <$> B.getContents

writePCM :: [Double] -> IO ()
writePCM = B.putStr . toPCM

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["enc_pcm"] ->
      (toPCM . fskEncodePCM48 . tFromBits . bytesToBits . B.unpack <$> B.getContents) >>=
      B.putStr
    ["enc"] ->
      (toLazyByteString . fskEncodeTone48 . tFromBits . bytesToBits . B.unpack <$>
       B.getContents) >>=
      B.putStr
    ["dec"] ->
      (B.pack . bitsToBytes . tToBits . fskDecode . fromPCM <$> B.getContents) >>=
      B.putStr
    ["lowpass6"] -> (lowPass6db 400 <$> readPCM) >>= writePCM
    ["lowpass12"] -> (lowPass12db 400 <$> readPCM) >>= writePCM
    ["lowpass24"] -> (lowPass24db 400 <$> readPCM) >>= writePCM
    ["highpass6"] -> (highPass6db 400 <$> readPCM) >>= writePCM
    ["highpass12"] -> (highPass12db 400 <$> readPCM) >>= writePCM
    ["highpass24"] -> (highPass24db 400 <$> readPCM) >>= writePCM
    ["bandpass6"] -> (bandPass6db 400 <$> readPCM) >>= writePCM
    ["bandpass12"] -> (bandPass12db 400 <$> readPCM) >>= writePCM
    ["bandpass24"] -> (bandPass24db 400 <$> readPCM) >>= writePCM
    ["lock"] -> (lockSignal <$> readPCM) >>= writePCM
    ["fun"] -> (fun <$> readPCM) >>= writePCM
    {-["dft"] -> (lockSignal' <$> readPCM) >>= writePCM-}
    _ -> putStrLn "Usage: fsk <enc_pcm|enc|dec|lowpass6|lowpass12|lowpass24>"
      
