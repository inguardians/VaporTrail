{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module VaporTrail.Codec.LDPC where

import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Linear
import Linear.V
import Control.Monad.Zip
import Data.Bits (xor)

import System.Random

chunkSize :: Int
chunkSize = 1024

n :: Int
n = 2048

k :: Int
k = 1024


newtype GF2 = GF2 { unGF2 :: Bool }
  deriving (Eq, Read, Show, Random)

-- Mathematics in GF(2)
instance Num GF2 where
  negate = id
  abs = id
  signum = id
  fromInteger x = GF2 ((abs x `mod` 2) == 1)
  (+) (GF2 a) (GF2 b) = GF2 (xor a b)
  (-) (GF2 a) (GF2 b) = GF2 (xor a b)
  (*) (GF2 a) (GF2 b) = GF2 (a && b)

-- https://en.wikipedia.org/wiki/Low-density_parity-check_code
-- Matrix in row-major to make codegen faster by being in column major
baseMatrix :: V 1024 (V 1024 GF2)
baseMatrix =
  let gen = mkStdGen 0xBEEF
      fullData = Vector.fromListN (chunkSize * chunkSize) (randoms gen)
      sliceAt i = Vector.slice (i * chunkSize) chunkSize fullData
  in V (Vector.generate chunkSize (\i -> V (sliceAt i)))

baseIdent :: V 1024 (V 1024 GF2)
baseIdent = identity

-- Identity on the right
checkMatrix :: V 1024 (V 2048 GF2)
checkMatrix =
  mzipWith (\a b -> V (toVector a Vector.++ toVector b)) baseMatrix baseIdent

-- Check matrix transposed and negated, with identity on left
-- Negating does nothing though because it's in GF(2)
generatorMatrix :: V 1024 (V 2048 GF2)
generatorMatrix =
  mzipWith
    (\a b -> V (toVector a Vector.++ toVector b))
    baseIdent
    (transpose baseMatrix)

-- | In this code, remember that in GF(2), addition is xor and multiplication is &&.
-- In other words, everything is mod-2.
encodeChunk :: V 1024 GF2 -> V 2048 GF2
encodeChunk vec = vec *! generatorMatrix

decodeChunk :: V 2048 GF2 -> Maybe (V 1024 GF2)
decodeChunk = undefined


validate :: V 2048 GF2 -> V 1024 GF2
validate vec = checkMatrix !* vec

-- | checkMatrix !* data should result in a vector of all falses. If any
-- bit is true, the whole thing is invalid.
isValid :: V 2048 GF2 -> Bool
isValid vec = not (any unGF2 (validate vec))


{-solve :: V 2048 GF2 -> Maybe (V 1024 GF2)-}
{-solve v =-}
  {-let-}
    {-constraints = validate v-}


