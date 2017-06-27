module VaporTrail.Codec.UMsg where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString.Lazy

import Data.Int


data Block = Block
  { blockID :: Int64
  , blockData :: ByteString
  , blockHash :: ByteString
  , blockParentHash :: ByteString
  } deriving (Eq, Read, Show)


fullBlockData :: Block -> ByteString
fullBlockData = undefined

hashData :: ByteString -> ByteString
hashData = undefined


