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
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Semigroup
import Data.Word

newtype DataChunk = DataChunk
  { getDataChunk :: Lazy.ByteString
  } deriving (Eq, Read, Show)

emptyDataChunk :: DataChunk
emptyDataChunk = DataChunk Lazy.empty

data FecChunk = FecChunk
  { fecChunkIndex :: Int
  , fecChunkBytes :: Strict.ByteString
  } deriving (Eq, Read, Show)

dataSize :: Int
dataSize = 128

serializeDataChunk :: Int -> DataChunk -> Lazy.ByteString
serializeDataChunk size (DataChunk bytes) =
  let dataLength = fromIntegral (Lazy.length bytes)
      padding =
        if dataLength < size
          then stimesMonoid (dataSize - dataLength) (Builder.word8 0)
          else mempty
  in Builder.toLazyByteString
       (Builder.int32LE (fromIntegral dataLength) <>
        Builder.lazyByteString bytes <>
        padding)

deserializeDataChunk :: Lazy.ByteString -> DataChunk
deserializeDataChunk input =
  flip Binary.runGet input $ do
    dataLength <- Binary.getInt32le
    msg <- Binary.getLazyByteString (fromIntegral dataLength)
    return (DataChunk msg)

dataChunkSize :: Int -> Int
dataChunkSize size = 4 + size

fecChunkSize :: Int -> Int
fecChunkSize size = 4 + Crypto.hashDigestSize Crypto.SHA256 + dataChunkSize size

serializeFecChunk :: FecChunk -> Lazy.ByteString
serializeFecChunk fecChunk =
  let body =
        Builder.toLazyByteString
          (Builder.int32LE (fromIntegral (fecChunkIndex fecChunk)) <>
           Builder.byteString (fecChunkBytes fecChunk))
      chunkDigest = Crypto.hashlazy body :: Crypto.Digest Crypto.SHA256
  in Builder.toLazyByteString
       (Builder.lazyByteString body <>
        Builder.byteString (ByteArray.convert chunkDigest))

deserializeFecChunk :: Int -> Lazy.ByteString -> Maybe FecChunk
deserializeFecChunk chunkSize input =
  case flip Binary.runGetOrFail input $ do
         chunkIndex <- Binary.lookAhead Binary.getInt32le
         chunkIndexBytes <- Binary.getByteString 4
         chunkMessage <- Binary.getByteString (dataChunkSize chunkSize)
         chunkDigest <-
           Binary.getByteString (Crypto.hashDigestSize Crypto.SHA256)
         let expectedDigest =
               Crypto.hashlazy
                 (Lazy.append
                    (Lazy.fromStrict chunkIndexBytes)
                    (Lazy.fromStrict chunkMessage)) :: Crypto.Digest Crypto.SHA256
         return
           (if chunkDigest == ByteArray.convert expectedDigest
              then Just (FecChunk (fromIntegral chunkIndex) chunkMessage)
              else Nothing) of
    Left _ -> Nothing
    Right (_, _, x) -> x

encodeChunks :: Word8 -> Word8 -> [DataChunk] -> [FecChunk]
encodeChunks k n input =
  let fecParams = FEC.fec (fromIntegral k) (fromIntegral n)
      padChunks xs = take (fromIntegral k) (xs <> repeat emptyDataChunk)
      serialize = map (Lazy.toStrict . serializeDataChunk dataSize)
      redundancy = FEC.encode fecParams
      psi (i, xs) =
        case splitAt (fromIntegral k) xs of
          ([], _) -> Nothing
          (h, t) ->
            let serializedChunks = serialize (padChunks h)
                redundancyChunks = redundancy serializedChunks
                fecChunks =
                  zipWith FecChunk [i ..] (serializedChunks <> redundancyChunks)
            in Just (fecChunks, (i + fromIntegral n, t))
  in concat (unfoldr psi (0 :: Int, input))

reassembleChunks :: Word8 -> Word8 -> [FecChunk] -> [DataChunk]
reassembleChunks k n =
  let fecParams = FEC.fec (fromIntegral k) (fromIntegral n)
      increasing x y =
        (mod (fecChunkIndex x) (fromIntegral n) <
         mod (fecChunkIndex y) (fromIntegral n))
      unpackChunk (FecChunk idx bytes) = (idx `mod` fromIntegral n, bytes)
      reassemble xs =
        case take (fromIntegral k) (map unpackChunk xs) of
          inChunks
            | length inChunks == fromIntegral k -> FEC.decode fecParams inChunks
          _ -> []
  in map (deserializeDataChunk . Lazy.fromStrict) .
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

toFecChunks :: [Word8] -> [FecChunk]
toFecChunks =
  mapMaybe (deserializeFecChunk dataSize . Lazy.pack) .
  chunksOf (fecChunkSize dataSize)

fromFecChunks :: [FecChunk] -> [Word8]
fromFecChunks = foldMap (Lazy.unpack . serializeFecChunk)

fecChunkSerialization :: Iso' [FecChunk] [Word8]
fecChunkSerialization = iso fromFecChunks toFecChunks

fec :: Word8 -> Word8 -> Iso' [Word8] [Word8]
fec k n =
  let dataChunks = iso toDataChunks fromDataChunks
      fecChunks = iso (encodeChunks k n) (reassembleChunks k n)
  in dataChunks . fecChunks . fecChunkSerialization . iso (++ replicate 64 0) id
