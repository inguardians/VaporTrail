module VaporTrail.Codec.UCode (ucode) where

import VaporTrail.Codec.Type
import Data.Machine
import Data.Machine.Group
import Control.Monad

data TWord = Sync | One | Zero | Empty deriving (Eq,Read,Show)

tChunkSize :: Int
tChunkSize = 256

tAmp :: Fractional a => TWord -> a
tAmp Sync = 5 / 5
tAmp One = 3 / 5
tAmp Zero = 2 / 5
tAmp Empty = 0

tFromAmp :: Float -> TWord
tFromAmp x
  | x >= 0.8 = Sync
  | x >= 0.5 = One
  | x >= 0.1 = Zero
  | otherwise = Empty

tToBit :: TWord -> Maybe Bool
tToBit Sync = Nothing
tToBit One = Just True
tToBit Zero = Just False
tToBit Empty = Nothing

tToBits :: Process TWord Bool
tToBits =
  construct . forever $ do
    word <- await
    forM_ (tToBit word) yield

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

decode :: Process Float TWord
decode =
  let notEmpty x = tFromAmp (abs x) /= Empty
      polarity x = x < 0
      eqPolarity x y = polarity x == polarity y
  in mapping tFromAmp <~ groupingOn eqPolarity (largest <~ mapping abs) <~
     filtered notEmpty

encode :: Process TWord Float
encode =
  construct . forever $ do
    positive <- await
    yield (tAmp positive)
    negative <- await
    yield (-(tAmp negative))


ucode :: Codec Bool Float
ucode = Codec {codecEnc = encode <~ tFromBits, codecDec = tToBits <~ decode}
