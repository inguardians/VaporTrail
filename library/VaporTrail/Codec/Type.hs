{-# LANGUAGE RankNTypes #-}
module VaporTrail.Codec.Type
  ( Codec(..)
  , codec
  , flipCodec
  , codecEnc
  , codecDec
  , codecLmap
  , codecRmap
  , codecBimap
  , liftProcChunked
  , liftCodecChunked
  , lowerProcChunked
  , lowerCodecChunked
  ) where

import Control.Monad
import Data.Machine
import qualified Data.Vector.Generic as Vector
import Data.Vector.Generic (Vector)
import Control.Category
import Prelude hiding ((.), id)


newtype Codec a b = Codec
  { runCodec :: forall r. (Process a b -> Process b a -> r) -> r
  }

flipCodec :: Codec a b -> Codec b a
flipCodec (Codec x) = Codec (\f -> x (\a b -> f b a))
{-# INLINABLE flipCodec #-}

codec :: Process a b -> Process b a -> Codec a b
codec enc dec = Codec (\f -> f enc dec)
{-# INLINABLE codec #-}

codecEnc :: Codec a b -> Process a b
codecEnc (Codec x) = x (\a _ -> a)
{-# INLINABLE codecEnc #-}

codecDec :: Codec a b -> Process b a
codecDec (Codec x) = x (\_ b -> b)
{-# INLINABLE codecDec #-}

instance Category Codec where
  id = Codec (\f -> f echo echo)
  {-# INLINABLE id #-}
  secondCodec . firstCodec =
    Codec
      (\f ->
         f
           (codecEnc firstCodec ~> codecEnc secondCodec)
           (codecDec secondCodec ~> codecDec firstCodec))
  {-# INLINABLE (.) #-}


codecLmap :: Process a b -> Process b a -> Codec b c -> Codec a c
codecLmap f g x = codec f g >>> x
{-# INLINABLE codecLmap #-}

codecRmap :: Process c d -> Process d c -> Codec b c -> Codec b d
codecRmap f g x = x >>> codec f g
{-# INLINABLE codecRmap #-}

codecBimap ::
     Process a b
  -> Process b a
  -> Process c d
  -> Process d c
  -> Codec b c
  -> Codec a d
codecBimap ab ba cd dc = codecLmap ab ba >>> codecRmap cd dc
{-# INLINABLE codecBimap #-}

toChunks :: Vector v a => Int -> Process a (v a)
toChunks n =
  repeatedly $ do
    xs <- replicateM n await
    yield (Vector.fromListN n xs)

fromChunks :: Vector v a => Process (v a) a
fromChunks =
  repeatedly $ do
    chunk <- await
    Vector.forM_ chunk yield

chunks :: Vector v a => Int -> Codec a (v a)
chunks n = codec (toChunks n ) fromChunks

-- | Lifts a process to operate on chunks. Less efficient than writing
-- it to work with chunks from the start. The second argument specifies
-- the size of the output chunks
liftProcChunked :: (Vector v0 a, Vector v1 b) => Process a b -> Int -> Process (v0 a) (v1 b)
liftProcChunked p n = fromChunks ~> p ~> toChunks n

-- | Lifts a codec to operate on chunks. Less efficient than writing
-- it to work with chunks from the start.
liftCodecChunked :: (Vector v0 a, Vector v1 b) => Codec a b -> Int -> Codec (v0 a) (v1 b)
liftCodecChunked x n = flipCodec (chunks n) >>> x >>> chunks n

-- Codec (v a) a -> Codec a b -> Codec a (v b)

-- | Lowers a process to work on individual elements. The second argument
-- specifies the size of chunks provided to the process
lowerProcChunked :: (Vector v0 a, Vector v1 b) => Process (v0 a) (v1 b) -> Int -> Process a b
lowerProcChunked p n = toChunks n ~> p ~> fromChunks

lowerCodecChunked :: (Vector v0 a, Vector v1 b) => Codec (v0 a) (v1 b) -> Int -> Codec a b
lowerCodecChunked x n = chunks n >>> x >>> flipCodec (chunks n)
