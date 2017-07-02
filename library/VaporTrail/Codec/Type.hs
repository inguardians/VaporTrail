{-# LANGUAGE RankNTypes #-}
module VaporTrail.Codec.Type
  ( Codec
  , codec
  , codecEnc
  , codecDec
  ) where

import Control.Lens

type Codec a b = Iso' a b

codec :: (a -> b) -> (b -> a) -> Codec a b
codec = iso
{-# INLINABLE codec #-}

codecEnc :: Codec a b -> (a -> b)
codecEnc f = view f
{-# INLINABLE codecEnc #-}

codecDec :: Codec a b -> (b -> a)
codecDec f = view (from f)
{-# INLINABLE codecDec #-}
