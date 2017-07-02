{-# LANGUAGE RankNTypes #-}
module VaporTrail.Codec.Chunk
  ( chunkLazy
  , chunkStrict
  ) where

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.Word
import VaporTrail.Codec.Type
import Data.Bifunctor
import Data.List

toChunksLazy :: Int -> [Word8] -> [Lazy.ByteString]
toChunksLazy chunkSize =
  let psi xs =
        case first Lazy.pack (splitAt chunkSize xs) of
          (chunk, t) ->
            if fromIntegral (Lazy.length chunk) == chunkSize
              then Just (chunk, t)
              else Nothing
  in unfoldr psi
{-# INLINABLE toChunksLazy #-}

fromChunksLazy :: [Lazy.ByteString] -> [Word8]
fromChunksLazy = foldMap Lazy.unpack
{-# INLINABLE fromChunksLazy #-}

toChunksStrict :: Int -> [Word8] -> [Strict.ByteString]
toChunksStrict chunkSize = map Lazy.toStrict . toChunksLazy chunkSize
{-# INLINABLE toChunksStrict #-}

fromChunksStrict :: [Strict.ByteString] -> [Word8]
fromChunksStrict = foldMap Strict.unpack
{-# INLINABLE fromChunksStrict #-}

chunkLazy :: Int -> Codec [Word8] [Lazy.ByteString]
chunkLazy chunkSize = codec (toChunksLazy chunkSize) fromChunksLazy
{-# INLINABLE chunkLazy #-}

chunkStrict :: Int -> Codec [Word8] [Strict.ByteString]
chunkStrict chunkSize = codec (toChunksStrict chunkSize) fromChunksStrict
{-# INLINABLE chunkStrict #-}
