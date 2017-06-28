{-# LANGUAGE RankNTypes #-}
module VaporTrail.Codec.Type
  ( Codec(..)
  , toChunks
  , fromChunks
  , liftProcChunked
  , liftCodecChunked
  , lowerProcChunked
  , lowerCodecChunked
  ) where

import Control.Monad
import Data.Machine
import qualified Data.Vector.Generic as Vector
import Data.Vector.Generic (Vector)

data Codec a b = Codec
  { codecEnc :: Process a b
  , codecDec :: Process b a
  }

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

-- | Lifts a process to operate on chunks. Less efficient than writing
-- it to work with chunks from the start. The second argument specifies
-- the size of the output chunks
liftProcChunked :: (Vector v0 a, Vector v1 b) => Process a b -> Int -> Process (v0 a) (v1 b)
liftProcChunked p n = toChunks n <~ p <~ fromChunks

-- | Lifts a codec to operate on chunks. Less efficient than writing
-- it to work with chunks from the start.
liftCodecChunked :: (Vector v0 a, Vector v1 b) => Codec a b -> Int -> Codec (v0 a) (v1 b)
liftCodecChunked codec n =
  Codec
  { codecEnc = liftProcChunked (codecEnc codec) n
  , codecDec = liftProcChunked (codecDec codec) n
  }

-- | Lowers a process to work on individual elements. The second argument
-- specifies the size of chunks provided to the process
lowerProcChunked :: (Vector v0 a, Vector v1 b) => Process (v0 a) (v1 b) -> Int -> Process a b
lowerProcChunked p n = fromChunks <~ p <~ toChunks n

lowerCodecChunked :: (Vector v0 a, Vector v1 b) => Codec (v0 a) (v1 b) -> Int -> Codec a b
lowerCodecChunked codec n =
  Codec
  { codecEnc = lowerProcChunked (codecEnc codec) n
  , codecDec = lowerProcChunked (codecDec codec) n
  }
