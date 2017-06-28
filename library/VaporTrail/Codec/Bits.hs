{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module VaporTrail.Codec.Bits (Endianness(..), bits, bitsLE, bitsBE) where

import Control.Monad
import Data.Bits
import Data.Machine
import VaporTrail.Codec.Type

data Endianness
  = LittleEndian
  | BigEndian
  deriving (Eq, Read, Show, Enum, Ord)

bitIndex :: Endianness -> Int -> Int -> Int
bitIndex LittleEndian _ = id
bitIndex BigEndian numBits = \n -> numBits - 1 - n

toBits :: forall b. FiniteBits b => Endianness -> Process b Bool
toBits endian =
  let numBits = finiteBitSize (zeroBits :: b)
      yieldBit x n = yield (testBit x (bitIndex endian numBits n))
  in repeatedly $ do
       x <- await
       forM_ [0 .. numBits - 1] (yieldBit x)
  
fromBits :: forall b. FiniteBits b => Endianness -> Process Bool b
fromBits endian =
  let numBits = finiteBitSize (zeroBits :: b)
      putBit x n True = setBit x (bitIndex endian numBits n)
      putBit x _ False = x
      awaitBit x n = do
        b <- await
        return (putBit x n b)
  in repeatedly $ do
       x <- foldM awaitBit zeroBits [0 .. numBits - 1]
       yield x
      
bits :: FiniteBits b => Endianness -> Codec b Bool
bits endian = Codec {codecEnc = toBits endian, codecDec = fromBits endian}

bitsBE :: FiniteBits b => Codec b Bool
bitsBE = bits BigEndian

bitsLE :: FiniteBits  b => Codec b Bool
bitsLE = bits LittleEndian
