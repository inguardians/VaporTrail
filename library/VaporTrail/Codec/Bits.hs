{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module VaporTrail.Codec.Bits (Endianness(..), bits, bitsLE, bitsBE) where

import Control.Lens (Iso', iso)
import Data.Bits
import Data.List
import Data.Semigroup

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
        let (h, t) = splitAt numBits xs
            (y, len) = foldl' putBit (zeroBits, 0) h
        in if len == numBits
             then Just (y, t)
             else Nothing
  in unfoldr psi
{-# INLINABLE fromBits #-}
      
bits :: FiniteBits b => Endianness -> Iso' [b] [Bool]
bits endian = iso (toBits endian) (fromBits endian)
{-# INLINABLE bits #-}

bitsBE :: FiniteBits b => Iso' [b] [Bool]
bitsBE = bits BigEndian
{-# INLINABLE bitsBE #-}

bitsLE :: FiniteBits  b => Iso' [b] [Bool]
bitsLE = bits LittleEndian
{-# INLINABLE bitsLE #-}
