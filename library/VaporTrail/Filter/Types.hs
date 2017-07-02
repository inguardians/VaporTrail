module VaporTrail.Filter.Types
  ( HasSampleRate(..)
  ) where

class HasSampleRate a where
  getSampleRate :: a -> Int

instance HasSampleRate Int where
  getSampleRate = id
