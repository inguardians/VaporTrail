{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module VaporTrail.Codec.Bits (Endianness(..), bits, bitsLE, bitsBE) where

import Data.Bits
import Data.Semigroup
import VaporTrail.Codec.Type
import Data.List

data Endianness
  = LittleEndian
  | BigEndian
  deriving (Eq, Read, Show, Enum, Ord)

bitIndex :: Endianness -> Int -> Int -> Int
bitIndex LittleEndian _ = id
bitIndex BigEndian numBits = \n -> numBits - 1 - n
{-# INLINABLE bitIndex #-}

toBits :: forall b. FiniteBits b => Endianness -> [b] -> [Bool]
toBits endian =
  let numBits = finiteBitSize (zeroBits :: b)
      getBit x n = testBit x (bitIndex endian numBits n)
      phi x xs = map (getBit x) [0 .. numBits - 1] <> xs
  in foldr phi []
{-# INLINABLE toBits #-}

fromBits :: forall b. FiniteBits b => Endianness -> [Bool] -> [b]
fromBits endian =
  let numBits = finiteBitSize (zeroBits :: b)
      putBit (x, n) True = (setBit x (bitIndex endian numBits n), n + 1)
      putBit (x, n) False = (x, n + 1)
      psi xs =
        case splitAt numBits xs of
          (h, t)
            | length h == numBits ->
              Just (fst (foldl' putBit (zeroBits, 0) h), t)
          _ -> Nothing
  in unfoldr psi
{-# INLINABLE fromBits #-}
      
bits :: FiniteBits b => Endianness -> Codec [b] [Bool]
bits endian = codec (toBits endian) (fromBits endian)
{-# INLINABLE bits #-}

bitsBE :: FiniteBits b => Codec [b] [Bool]
bitsBE = bits BigEndian
{-# INLINABLE bitsBE #-}

bitsLE :: FiniteBits  b => Codec [b] [Bool]
bitsLE = bits LittleEndian
{-# INLINABLE bitsLE #-}
