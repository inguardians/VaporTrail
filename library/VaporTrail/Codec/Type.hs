{-# LANGUAGE RankNTypes #-}
module VaporTrail.Codec.Type
  ( Codec(..)
  ) where

import Data.Machine

data Codec a b = Codec
  { codecEnc :: Process a b
  , codecDec :: Process b a
  }

