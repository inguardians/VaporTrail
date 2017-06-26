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

-- https://en.wikipedia.org/wiki/Low-pass_filter#Discrete-time_realization
lowPass6db :: Floating a => a -> [a] -> [a]
lowPass6db _ [] = []
lowPass6db hz (x:xs) = scanl' f (x * a) xs
  where
    dt = 1 / fromIntegral sampleRate
    rc = 1 / (2 * pi * hz)
    a = dt / (dt + rc)
    f acc x = acc + (x - acc) * a

-- https://en.wikipedia.org/wiki/High-pass_filter#Algorithmic_implementation
highPass6db :: Floating a => a -> [a] -> [a]
highPass6db _ [] = []
highPass6db hz xs = scanl' f (head x) (zipWith subtract xs (tail xs))
  where
    dt = 1 / fromIntegral sampleRate
    rc = 1 / (2 * pi * hz)
    a = dt / (dt + rc)
    f acc dx = a * (acc + dx)

bandPass6db :: Floating a => a -> [a] -> [a]
bandPass6db hz = highPass6db hz . lowPass6db hz


-- Order 1 = 6db
-- Order 2 = 12db
-- Order 3 = 24db
-- etc...
order :: Int -> (a -> a) -> a -> a
order n f = appEndo (stimesMonoid (2 ^ (n - 1)) (Endo f))

lowPass :: Floating a => Int -> a -> [a] -> [a]
lowPass n hz = order n (lowPass6db hz)

lowPass12db :: Floating a => a -> [a] -> [a]
lowPass12db = lowPass 2

lowPass24db :: Floating a => a -> [a] -> [a]
lowPass24db = lowPass 3

highPass :: Floating a => Int -> a -> [a] -> [a]
highPass n hz = order n (highPass6db hz)

highPass12db :: Floating a => a -> [a] -> [a]
highPass12db = highPass 2

highPass24db :: Floating a => a -> [a] -> [a]
highPass24db = highPass 3

bandPass :: Floating a => Int -> a -> [a] -> [a]
bandPass n hz = order n (bandPass6db hz)

bandPass12db :: Floating a => a -> [a] -> [a]
bandPass12db = bandPass 2

bandPass24db :: Floating a => a -> [a] -> [a]
bandPass24db = bandPass 3

data TWord = Sync | One | Zero | Empty deriving (Eq,Read,Show)

tAmp :: Fractional a => TWord -> a
tAmp Sync = 5 / 5
tAmp One = 3 / 5
tAmp Zero = 2 / 5
tAmp Empty = 1 / 5


sampleRate = 48000

dataRate = 2000

-- https://en.wikipedia.org/wiki/Goertzel_algorithm
goertzel :: RealFloat a => a -> Int -> [a] -> Complex a
goertzel bin dftSize samples = y !! dftSize
  where
    w0 = 2 * pi * (bin / fromIntegral dftSize)
    e = exp 1
    x = take dftSize samples ++ repeat 0
    s = zipWith3 (\x s1 s2 -> x + 2 * cos w0 * s1 - s2) x (0 : s) (0 : 0 : s)
    y = zipWith (\s s1 -> (s :+ 0) - e ** (0 :+ (-w0)) * (s1 :+ 0)) s (tail s)

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
  mapMaybe tToBit . concat . filter ((== tChunkSize) . length) . groupBy g
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
      
