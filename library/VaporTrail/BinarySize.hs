{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
module VaporTrail.BinarySize
  ( BinarySize(..)
  , BinarySizeFixed(..)
  ) where

import Data.Binary (Binary)
import Data.Proxy

class Binary a => BinarySize a where
  binarySize :: a -> Int

  default binarySize :: BinarySizeFixed a => a -> Int
  binarySize x = binarySizeFixed (x <$ Proxy)

class BinarySize a => BinarySizeFixed a where
  binarySizeFixed :: forall proxy. proxy a -> Int

instance (BinarySize a, BinarySize b) => BinarySize (a, b) where
  binarySize (x, y) = binarySize x + binarySize y

instance forall a b. (BinarySizeFixed a, BinarySizeFixed b) => BinarySizeFixed (a, b) where
  binarySizeFixed _ =
    binarySizeFixed (Proxy :: Proxy a) + binarySizeFixed (Proxy :: Proxy b)
