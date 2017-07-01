{-# LANGUAGE RankNTypes #-}
-- | Forward error correction using the `fec` library, a haskell binding of 
-- `zfec`
module VaporTrail.Codec.FEC (fec) where

import qualified Codec.FEC as FEC
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Machine hiding (zipWith)
import Data.Machine.Group
import Data.Word
import VaporTrail.Codec.Type
import Data.Semigroup

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Builder as Builder
import qualified Data.Binary.Get as Binary
import qualified Data.ByteArray as ByteArray
import qualified Crypto.Hash as Crypto

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
  flip Binary.runGet input $ do
    chunkIndex <- Binary.lookAhead Binary.getInt32le
    chunkIndexBytes <- Binary.getByteString 4
    chunkMessage <- Binary.getByteString (dataChunkSize chunkSize)
    chunkDigest <- Binary.getByteString (Crypto.hashDigestSize Crypto.SHA256)
    let expectedDigest =
          Crypto.hashlazy
            (Lazy.append
               (Lazy.fromStrict chunkIndexBytes)
               (Lazy.fromStrict chunkMessage)) :: Crypto.Digest Crypto.SHA256
    return
      (if chunkDigest == ByteArray.convert expectedDigest
         then Just (FecChunk (fromIntegral chunkIndex) chunkMessage)
         else Nothing)
          
encodeChunks :: Word8 -> Word8 -> Process DataChunk FecChunk
encodeChunks k n =
  let fecParams = FEC.fec (fromIntegral k) (fromIntegral n)
      runEncoder i = do
        inChunks <- await
        let paddedChunks =
              take (fromIntegral k) (inChunks ++ repeat emptyDataChunk)
            serializedChunks =
              map (Lazy.toStrict . serializeDataChunk dataSize) paddedChunks
            redundancyChunks = FEC.encode fecParams serializedChunks
        forM_ (zipWith FecChunk [i ..] serializedChunks) yield
        forM_ (zipWith FecChunk [(i + fromIntegral k) ..] redundancyChunks) yield
        runEncoder (i + fromIntegral n)
  in buffered (fromIntegral k) ~> construct (runEncoder 0)


reassembleChunks :: Word8 -> Word8 -> Process FecChunk DataChunk
reassembleChunks k n =
  let fecParams = FEC.fec (fromIntegral k) (fromIntegral n)
      increasing x y =
        (mod (fecChunkIndex x) (fromIntegral n) <
         mod (fecChunkIndex y) (fromIntegral n))
      takeChunks xs =
        (do x <- await
            let chunkTup = (fecChunkIndex x `mod` (fromIntegral n), fecChunkBytes x)
            takeChunks (chunkTup : xs)) <|>
        return xs
      decoder =
        construct $ do
          inChunks <- fmap (take (fromIntegral k)) (takeChunks [])
          if length inChunks /= fromIntegral k
            then return ()
            else forM_
                   (FEC.decode fecParams inChunks)
                   (yield . deserializeDataChunk . Lazy.fromStrict)
  in groupingOn increasing decoder


toDataChunks :: Process Word8 DataChunk
toDataChunks = buffered dataSize ~> mapping (DataChunk . Lazy.pack)

fromDataChunks :: Process DataChunk Word8
fromDataChunks = mapping (Lazy.unpack . getDataChunk) ~> flattened

fecChunkSerialization :: Codec FecChunk Word8
fecChunkSerialization =
  codec
    (mapping (Lazy.unpack . serializeFecChunk) ~> flattened)
    (buffered (fecChunkSize dataSize) ~>
     mapping (deserializeFecChunk dataSize . Lazy.pack) ~>
     flattened)

fec :: Word8 -> Word8 -> Codec Word8 Word8
fec k n =
  let dataChunks = codec toDataChunks fromDataChunks
      fecChunks = codec (encodeChunks k n) (reassembleChunks k n)
  in dataChunks >>> fecChunks >>> fecChunkSerialization

