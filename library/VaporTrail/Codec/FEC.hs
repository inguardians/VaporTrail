{-# LANGUAGE RankNTypes #-}

-- | Forward error correction using the `fec` library, a haskell binding of
-- `zfec`
module VaporTrail.Codec.FEC
  ( fec
  ) where

import qualified Codec.FEC as FEC
import Control.Lens (Iso', iso)
import qualified Crypto.Hash as Crypto
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Semigroup
import Data.Word
import Control.Monad


dataSize :: Int
dataSize = 128

newtype DataChunk = DataChunk
  { getDataChunk :: Lazy.ByteString
  } deriving (Eq, Read, Show)

instance Binary DataChunk where
  put (DataChunk bytes) = do
    let dataLen = fromIntegral (Lazy.length bytes)
    Binary.putWord8 (fromIntegral dataLen)
    Binary.putLazyByteString bytes
    when
      (dataLen < dataSize)
      (Binary.putLazyByteString
         (Lazy.replicate (fromIntegral (dataSize - dataLen)) 0))
  get = do
    dataLen <- Binary.getWord8
    bytes <- Binary.getLazyByteString (fromIntegral dataLen)
    when
      (fromIntegral dataLen < dataSize)
      (void
         (Binary.getLazyByteString
            (fromIntegral (dataSize - fromIntegral dataLen))))
    return (DataChunk bytes)

emptyDataChunk :: DataChunk
emptyDataChunk = DataChunk Lazy.empty


encodeChunks :: Word8 -> Word8 -> [DataChunk] -> [(Int, Strict.ByteString)]
encodeChunks k n input =
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
  in concat (unfoldr psi (0 :: Int, input))

reassembleChunks :: Word8 -> Word8 -> [(Int, Strict.ByteString)] -> [DataChunk]
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
  in map (Binary.runGet Binary.get . Lazy.fromStrict) .
     foldMap reassemble . groupBy increasing

toDataChunks :: [Word8] -> [DataChunk]
toDataChunks = map (DataChunk . Lazy.pack) . chunksOf dataSize

fromDataChunks :: [DataChunk] -> [Word8]
fromDataChunks =
  let psi [] = Nothing
      psi (DataChunk x:xs) =
        Just
          ( Lazy.unpack x
          , if fromIntegral (Lazy.length x) == dataSize
              then xs
              else [])
  in concat . unfoldr psi

fec :: Word8 -> Word8 -> Iso' [Word8] [(Int, Strict.ByteString)]
fec k n =
  let dataChunks = iso toDataChunks fromDataChunks
      fecChunks = iso (encodeChunks k n) (reassembleChunks k n)
  in dataChunks . fecChunks
