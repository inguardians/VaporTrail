{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Forward error correction using the `fec` library, a haskell binding of
-- `zfec`
module VaporTrail.Codec.FEC
  ( fec
  , FecChunk(..)
  , fecDataSize
  ) where

import qualified Codec.FEC as FEC
import Control.Lens (iso, Iso')
import Control.Monad
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.List
import Data.List.Split
import Data.Proxy
import Data.Semigroup
import Data.Word
import VaporTrail.BinarySize

fecDataSize :: Int
fecDataSize = 128

newtype FecChunk = FecChunk
  { getFecChunk :: (Int, Strict.ByteString) }
  deriving (Eq, Read, Show, Binary)

instance BinarySize FecChunk
instance BinarySizeFixed FecChunk where
  binarySizeFixed _ = 4 + binarySizeFixed (Proxy :: Proxy DataChunk)

newtype DataChunk = DataChunk
  { getDataChunk :: Lazy.ByteString
  } deriving (Eq, Read, Show)

instance BinarySize DataChunk
instance BinarySizeFixed DataChunk where
  binarySizeFixed _ = 1 + fecDataSize

instance Binary DataChunk where
  put (DataChunk bytes) = do
    let dataLen = fromIntegral (Lazy.length bytes)
    Binary.putWord8 (fromIntegral dataLen)
    Binary.putLazyByteString bytes
    when
      (dataLen < fecDataSize)
      (Binary.putLazyByteString
         (Lazy.replicate (fromIntegral (fecDataSize - dataLen)) 0))
  get = do
    dataLen <- Binary.getWord8
    bytes <- Binary.getLazyByteString (fromIntegral dataLen)
    when
      (fromIntegral dataLen < fecDataSize)
      (void
         (Binary.getLazyByteString
            (fromIntegral (fecDataSize - fromIntegral dataLen))))
    return (DataChunk bytes)

emptyDataChunk :: DataChunk
emptyDataChunk = DataChunk Lazy.empty

encodeChunks :: Word8 -> Word8 -> [[DataChunk]] -> [FecChunk]
encodeChunks k n =
  let fecParams = FEC.fec (fromIntegral k) (fromIntegral n)
      padChunks xs = take (fromIntegral k) (xs <> repeat emptyDataChunk)
      serialize = map (Lazy.toStrict . Binary.runPut . Binary.put)
      redundancy = FEC.encode fecParams
      psi (i, xs) =
        case splitAt (fromIntegral k) xs of
          ([], _) -> Nothing
          (h, t) ->
            let serializedChunks = serialize (padChunks h)
                redundancyChunks = redundancy serializedChunks
                fecChunks = zip [i ..] (serializedChunks <> redundancyChunks)
            in Just (fecChunks, (i + fromIntegral n, t))
      runPsi xs = concat (unfoldr psi (0 :: Int, xs))
  in map FecChunk . concatMap runPsi

reassembleChunks :: Word8 -> Word8 -> [FecChunk] -> [[DataChunk]]
reassembleChunks k n =
  let fecParams = FEC.fec (fromIntegral k) (fromIntegral n)
      increasing x y =
        (mod (fst x) (fromIntegral n) < mod (fst y) (fromIntegral n))
      unpackChunk (idx, bytes) = (idx `mod` fromIntegral n, bytes)
      reassemble xs =
        case take (fromIntegral k) (map unpackChunk xs) of
          inChunks
            | length inChunks == fromIntegral k -> FEC.decode fecParams inChunks
          _ -> []
      superChunks = groupBy increasing . map getFecChunk
      repack = map (Binary.runGet Binary.get . Lazy.fromStrict)
  in map repack . map reassemble . superChunks

toDataChunks :: [Word8] -> [DataChunk]
toDataChunks = map (DataChunk . Lazy.pack) . chunksOf fecDataSize

fromDataChunks :: [DataChunk] -> [Word8]
fromDataChunks = concatMap (Lazy.unpack . getDataChunk)

fec :: Word8 -> Word8 -> Iso' [[Word8]] [FecChunk]
fec k n =
  let dataChunks = iso (map toDataChunks) (map fromDataChunks)
      fecChunks = iso (encodeChunks k n) (reassembleChunks k n)
  in dataChunks . fecChunks
