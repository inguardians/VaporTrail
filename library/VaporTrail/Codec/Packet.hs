{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module VaporTrail.Codec.Packet (Packet(..), packetStream, packetStreamPacket) where

import Control.Lens (Iso', from, iso, view)
import Control.Monad
import qualified Crypto.Hash as Crypto
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.List
import VaporTrail.Codec.Bits
import VaporTrail.BinarySize
import GHC.Prim (coerce)
import Data.Proxy

newtype Packet a = Packet
  { packetData :: a
  } deriving (Eq, Read, Show)

packetChecksum :: Lazy.ByteString -> Strict.ByteString
packetChecksum pkt =
  ByteArray.convert (Crypto.hashlazy pkt :: Crypto.Digest Crypto.SHA256)

packetChecksumLength :: Int
packetChecksumLength = Crypto.hashDigestSize Crypto.SHA256

packetHeader :: Lazy.ByteString
packetHeader = "STRTDATA"

packetFooter :: Lazy.ByteString
packetFooter = "STOPDATA"

packetOverhead :: Int
packetOverhead = headerBytes + lengthBytes + checksumBytes + footerBytes
  where
    headerBytes = fromIntegral (Lazy.length packetHeader)
    lengthBytes = 4
    checksumBytes = packetChecksumLength
    footerBytes = fromIntegral (Lazy.length packetFooter)

instance Binary a => Binary (Packet a) where
  put pkt = do
    let bytes = Binary.runPut (Binary.put (packetData pkt))
    Binary.putLazyByteString packetHeader
    Binary.putWord32be (fromIntegral (Lazy.length bytes))
    Binary.putLazyByteString bytes
    Binary.putByteString (packetChecksum bytes)
    Binary.putLazyByteString packetFooter
  get = do
    pktHeader <- Binary.getLazyByteString (Lazy.length packetHeader)
    when (pktHeader /= packetHeader) (fail "Incorrect packet header")
    pktLength <- Binary.getWord32be
    pktBytes <-
      Binary.lookAhead (Binary.getLazyByteString (fromIntegral pktLength))
    pktData <- Binary.isolate (fromIntegral pktLength) Binary.get
    pktChecksum <- Binary.getByteString packetChecksumLength
    when
      (pktChecksum /= packetChecksum pktBytes)
      (fail "Invalid packet checksum")
    pktFooter <- Binary.getLazyByteString (Lazy.length packetFooter)
    when (pktFooter /= packetFooter) (fail "Incorrect packet footer")
    return (Packet pktData)

instance BinarySize a => BinarySize (Packet a) where
  binarySize x = binarySize (packetData x) + packetOverhead

instance forall a. BinarySizeFixed a => BinarySizeFixed (Packet a) where
  binarySizeFixed _ = (binarySizeFixed (Proxy :: Proxy a)) + packetOverhead

readPacket :: Binary a => Lazy.ByteString -> Either String (Int, Packet a)
readPacket bytes =
  case Binary.runGetOrFail Binary.get bytes of
    Left (_, _, err) -> Left err
    Right (_, bytesRead, packet) -> Right (fromIntegral bytesRead, packet)

writePacket :: Binary a => Packet a -> Builder.Builder
writePacket = Binary.execPut . Binary.put

readPacketStream :: Binary a => [Bool] -> [Packet a]
readPacketStream =
  let psi [] = Nothing
      psi xs =
        let bytes = Lazy.pack (view (from bitsBE) xs)
        in case readPacket bytes of
             Left _ -> Just ([], tail xs)
             Right (bytesRead, pkt) -> Just ([pkt], drop (bytesRead * 8) xs)
  in concat . unfoldr psi

writePacketStream :: Binary a => [Packet a] -> [Bool]
writePacketStream =
  view bitsBE . Lazy.unpack . Builder.toLazyByteString . foldMap writePacket

packetStreamPacket :: Binary a => Iso' [Packet a] [Bool]
packetStreamPacket = iso writePacketStream readPacketStream

packetStream ::
     forall a. Binary a
  => Iso' [a] [Bool]
packetStream =
  let coerceTo = coerce :: [a] -> [Packet a]
      coerceFrom = coerce :: [Packet a] -> [a]
  in iso coerceTo coerceFrom . packetStreamPacket
