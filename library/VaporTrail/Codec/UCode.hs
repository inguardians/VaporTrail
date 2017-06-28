module VaporTrail.Codec.UCode (ucode) where

import VaporTrail.Codec.Type
import Data.Machine
import Data.Machine.Group

data TWord = One | Zero | Empty deriving (Eq, Read, Show)

tRange :: TWord -> (Float, Float)
tRange One = (2/3, 1.0)
tRange Zero = (1/3, 2/3)
tRange Empty = (0, 1/3)

tAmp :: TWord -> Float
tAmp w =
  let (lo, hi) = tRange w
  in (lo + hi) / 2

tFromAmp :: Float -> TWord
tFromAmp x
  | x >= fst (tRange One) = One
  | x >= fst (tRange Zero) = Zero
  | otherwise = Empty

tToBit :: TWord -> Maybe Bool
tToBit Empty = Nothing
tToBit One = Just True
tToBit Zero = Just False

tToBits :: Process TWord Bool
tToBits =
  repeatedly $ do
    word <- await
    case tToBit word of
      Just x -> yield x
      _ -> return ()

tFromBit :: Bool -> TWord
tFromBit True = One
tFromBit False = Zero

tFromBits :: Process Bool TWord
tFromBits = mapping tFromBit

decode :: Process Float TWord
decode =
  let notEmpty x = tFromAmp (abs x) /= Empty
      polarity x = x < 0
      eqPolarity x y = polarity x == polarity y
  in mapping tFromAmp <~ groupingOn eqPolarity (largest <~ mapping abs) <~
     filtered notEmpty

encode :: Process TWord Float
encode =
  repeatedly $ do
    positive <- await
    yield (tAmp positive)
    negative <- await
    yield (-(tAmp negative))


ucode :: Codec Bool Float
ucode = Codec {codecEnc = encode <~ tFromBits, codecDec = tToBits <~ decode}
