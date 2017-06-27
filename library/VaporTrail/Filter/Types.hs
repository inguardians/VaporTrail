{-# LANGUAGE RankNTypes #-}
module VaporTrail.Filter.Types
  ( HasSampleRate(..)
  ) where

import Data.Machine

class HasSampleRate a where
  getSampleRate :: a -> Int

instance HasSampleRate Int where
  getSampleRate = id
