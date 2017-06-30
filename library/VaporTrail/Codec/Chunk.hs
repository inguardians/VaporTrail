{-# LANGUAGE RankNTypes #-}
module VaporTrail.Codec.Chunk
  ( chunkLazy
  , chunkStrict
  ) where

import Control.Monad
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.Machine
import Data.Word
import VaporTrail.Codec.Type

toChunksLazy :: Int -> Process Word8 Lazy.ByteString
toChunksLazy chunkSize =
  repeatedly $ do
    bytes <- replicateM chunkSize await
    return (Builder.toLazyByteString (foldMap Builder.word8 bytes))
{-# INLINABLE toChunksLazy #-}

fromChunksLazy :: Process Lazy.ByteString Word8
fromChunksLazy =
  repeatedly $ do
    bs <- await
    forM_ (Lazy.unpack bs) yield
{-# INLINABLE fromChunksLazy #-}

toChunksStrict :: Int -> Process Word8 Strict.ByteString
toChunksStrict chunkSize = toChunksLazy chunkSize ~> mapping Lazy.toStrict
{-# INLINABLE toChunksStrict #-}

fromChunksStrict :: Process Strict.ByteString Word8
fromChunksStrict =
  repeatedly $ do
    bs <- await
    forM_ (Strict.unpack bs) yield
{-# INLINABLE fromChunksStrict #-}

chunkLazy :: Int -> Codec Word8 Lazy.ByteString
chunkLazy chunkSize = codec (toChunksLazy chunkSize) fromChunksLazy
{-# INLINABLE chunkLazy #-}

chunkStrict :: Int -> Codec Word8 Strict.ByteString
chunkStrict chunkSize = codec (toChunksStrict chunkSize) fromChunksStrict
{-# INLINABLE chunkStrict #-}
